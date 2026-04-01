{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Parsing of PDF XRefs using 'Text.Grampa.PEG.Backtrack'.
--
-- References: PDF 32000-1:2008 (PDF 1.7 specification):
--
-- * §7.5 – File structure (cross-reference tables and streams, incremental updates)

module Text.FDF.PDF.XRef (parseXRefChain) where

import Control.Applicative ((<|>))
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import Data.Char (isDigit)
import Data.Int (Int64)
import Data.List (intercalate)
import qualified Data.IntMap.Strict as IntMap
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Monoid.Instances.ByteString.UTF8 (ByteStringUTF8 (ByteStringUTF8))
import Rank2 qualified
import Text.Grampa (InputParsing (string), InputCharParsing (..),
                    ParseFailure (..), FailureDescription (..))
import Text.Grampa.PEG.Backtrack qualified as PEG

import Text.FDF.PDF.Decompress (decompressStream)
import Text.FDF.PDF.Parse (parseDict)
import Text.FDF.PDF.Types

-- ---------------------------------------------------------------------------
-- Parser type

-- | Backtracking PEG parser for XRef fragments.  The input stream is
-- 'ByteStringUTF8', which provides a safe character interface (via
-- 'takeCharsWhile' etc.) for the ASCII-structured PDF syntax.
type XRefParser = PEG.Parser (Rank2.Only PDFValue) ByteStringUTF8

-- | Run an 'XRefParser', returning the result and the remaining (unconsumed)
-- input on success, or an error message on failure.
runXRefParser :: XRefParser a -> ByteString -> Either String (a, ByteString)
runXRefParser p input = case PEG.applyParser p (ByteStringUTF8 input) of
  PEG.Parsed v (ByteStringUTF8 rest) -> Right (v, rest)
  PEG.NoParse (ParseFailure _ (FailureDescription descs lits) errs) ->
    Left $ case descs ++ map (BSC.unpack . unwrapBS) lits ++ errs of
      []   -> "Parse failure"
      msgs -> "Expected: " ++ intercalate ", " msgs

-- | Unwrap 'ByteStringUTF8' to the underlying 'ByteString'.
unwrapBS :: ByteStringUTF8 -> ByteString
unwrapBS (ByteStringUTF8 bs) = bs

-- ---------------------------------------------------------------------------
-- Character predicates and primitive parsers

-- | Is a character PDF whitespace?
isPDFWS :: Char -> Bool
isPDFWS c = c `elem` (" \t\r\n\f\0" :: String)

-- | Skip zero or more PDF whitespace characters.
pWS :: XRefParser ()
pWS = () <$ takeCharsWhile isPDFWS

-- | Skip one or more PDF whitespace characters.
pWS1 :: XRefParser ()
pWS1 = () <$ takeCharsWhile1 isPDFWS

-- | Skip trailing spaces and then a line ending (CR, LF, or CRLF).
-- Tolerates missing line endings: if none is present the parser still succeeds
-- without consuming additional input.
pLineEnd :: XRefParser ()
pLineEnd =
  () <$ takeCharsWhile (== ' ') <*
  ((string "\r\n" <|> string "\r" <|> string "\n") <|> pure "")

-- | Parse an unsigned decimal integer (one or more digits).
pUnsignedInt :: XRefParser Int
pUnsignedInt =
  fmap (maybe 0 fst . BSC.readInt . unwrapBS) (takeCharsWhile1 isDigit)

-- | Parse the full chain of cross-reference tables, following @/Prev@ and
-- @/XRefStm@ links.
--
-- Hybrid-reference PDFs (created by Adobe Acrobat for backward compatibility)
-- have a traditional @xref@ table *and* a @/XRefStm@ entry in the trailer
-- that points to a cross-reference stream containing additional objects.
-- Both sources are merged, with the main section taking precedence.
parseXRefChain :: ByteString -> Int64 -> Either String (XRef, Map ByteString PDFValue)
parseXRefChain bs off = do
  (xref, trailer) <- parseOneXRef bs off
  -- Merge any /XRefStm cross-reference stream (hybrid-reference PDFs).
  xref' <- case Map.lookup "XRefStm" trailer of
              Just (PDFInt stmOff) ->
                case parseXRefStream bs (fromIntegral stmOff) of
                  Left err        -> Left ("/XRefStm parse error: " <> err)
                  Right (stmXref, _) ->
                    -- Main xref takes precedence over the hybrid stream.
                    Right (IntMap.union xref stmXref)
              _ -> return xref
  -- Follow the /Prev chain to older xref sections.
  case Map.lookup "Prev" trailer of
    Just (PDFInt prev) -> do
      (prevXRef, _) <- parseXRefChain bs (fromIntegral prev)
      -- Newer (current) entries take precedence over older ones.
      return (IntMap.union xref' prevXRef, trailer)
    _ -> return (xref', trailer)

-- | Parse a single cross-reference section (either traditional table or
-- cross-reference stream) and return the XRef map plus trailer dictionary.
parseOneXRef :: ByteString -> Int64 -> Either String (XRef, Map ByteString PDFValue)
parseOneXRef bs off = do
  let chunk = BS.drop (fromIntegral off) bs
  if "xref" `BS.isPrefixOf` chunk
    then parseTraditionalXRef chunk
    else parseXRefStream bs off

-- ---------------------------------------------------------------------------
-- Traditional (table-based) cross-reference section

-- | Parse a traditional (table-based) cross-reference section and its trailer.
parseTraditionalXRef :: ByteString -> Either String (XRef, Map ByteString PDFValue)
parseTraditionalXRef raw = do
  -- Skip "xref" keyword and following whitespace.
  ((), afterXRef) <- runXRefParser (string "xref" *> pWS) raw
  (afterSubs, xref) <- parseSubsections afterXRef IntMap.empty
  -- Skip "trailer" keyword and following whitespace.
  ((), afterTrailer) <- runXRefParser (string "trailer" *> pWS) afterSubs
  case parseDict afterTrailer of
    Left err       -> Left ("Trailer dict parse error: " <> err)
    Right (td, _)  -> Right (xref, td)

-- | Parse zero or more xref subsections, stopping at "trailer".
parseSubsections :: ByteString -> XRef -> Either String (ByteString, XRef)
parseSubsections bs xref = do
  -- Drop any whitespace between subsections (or before "trailer").
  ((), bs') <- runXRefParser pWS bs
  if "trailer" `BS.isPrefixOf` bs'
    then Right (bs', xref)
    else do
      (bs'', entries) <- parseSubsection bs'
      parseSubsections bs'' (IntMap.union entries xref)

-- | Parse one xref subsection: @firstObj count\n@ followed by entries.
parseSubsection :: ByteString -> Either String (ByteString, XRef)
parseSubsection bs0 = do
  ((firstObj, count), r) <- runXRefParser pSubsectionHeader bs0
  let entries = IntMap.fromList
                  [ (firstObj + i, e)
                  | i <- [0 .. count - 1]
                  , Just e <- [parseXRefEntry (BS.take 20 (BS.drop (i * 20) r))]
                  ]
  Right (BS.drop (count * 20) r, entries)

-- | Parser for the @firstObj count@ header line of an xref subsection.
pSubsectionHeader :: XRefParser (Int, Int)
pSubsectionHeader = do
  firstObj <- pUnsignedInt
  pWS1
  count    <- pUnsignedInt
  pLineEnd
  return (firstObj, count)

-- | Parse one 20-byte xref entry.  Returns 'Nothing' for free entries.
parseXRefEntry :: ByteString -> Maybe XRefEntry
parseXRefEntry entry
  | BS.length entry >= 18 && BSC.index entry 17 == 'f' = Nothing
  | otherwise = case BSC.readInt (BS.take 10 entry) of
      Just (n, _) -> Just (XRefOffset (fromIntegral n))
      Nothing     -> Nothing  -- malformed entry, skip

-- ---------------------------------------------------------------------------
-- Stream parsing and decompression

-- | Parse an indirect stream object at the given byte offset in the file.
-- Returns the stream dictionary and the raw (possibly compressed) stream bytes.
parseStreamAt :: ByteString -> Int64 -> Either String (Map ByteString PDFValue, ByteString)
parseStreamAt bs off = do
  let chunk = BS.drop (fromIntegral off) bs
  -- Use PEG to skip "N G obj" and surrounding whitespace.
  ((), afterObj) <- runXRefParser pObjHeader chunk
  -- Parse the stream dictionary.
  (dict, rest) <- parseDict afterObj
  -- Use PEG to skip "stream" keyword and its mandatory EOL.
  ((), streamStart) <- runXRefParser pStreamKeyword rest
  -- /Length must be a direct integer in xref streams; may be indirect
  -- in object streams (resolved after xref is built, see loadFromObjStream).
  len <- case Map.lookup "Length" dict of
           Just (PDFInt n) -> Right n
           Just _          -> Left "Stream /Length is not a direct integer"
           Nothing         -> Left "Stream dict missing /Length"
  Right (dict, BS.take len streamStart)

-- | Parser that skips leading whitespace and the @N G obj@ header,
-- leaving the cursor just before the stream dictionary.
-- The object and generation numbers are parsed solely to advance past them;
-- they are not needed for stream extraction.
pObjHeader :: XRefParser ()
pObjHeader = do
  pWS
  _ <- takeCharsWhile1 isDigit   -- object number (advance past only)
  pWS1
  _ <- takeCharsWhile1 isDigit   -- generation number (advance past only)
  pWS1
  _ <- string "obj"
  pWS

-- | Parser that consumes the @stream@ keyword and exactly one EOL.
pStreamKeyword :: XRefParser ()
pStreamKeyword = do
  pWS
  _ <- string "stream"
  pLineEnd

-- ---------------------------------------------------------------------------
-- Cross-reference stream (PDF 1.5+)

-- | Parse a cross-reference stream at the given byte offset.
-- The xref stream dictionary doubles as the trailer dictionary.
parseXRefStream :: ByteString -> Int64 -> Either String (XRef, Map ByteString PDFValue)
parseXRefStream bs off = do
  (dict, rawBytes) <- parseStreamAt bs off
  -- Decompress stream data if needed.
  streamBytes <- decompressStream dict rawBytes
  -- /W field: widths (in bytes) of the three entry fields.
  wArr <- case Map.lookup "W" dict of
    Just (PDFArray ws) -> Right ws
    _ -> Left "XRef stream missing /W"
  ws <- mapM toInt wArr
  case ws of
    [w1, w2, w3] -> do
      -- /Size: total number of object slots.
      size <- toInt =<< maybe (Left "XRef stream missing /Size") Right (Map.lookup "Size" dict)
      -- /Index: subsection pairs [firstObj count ...]; default is [0 /Size].
      let subsections = case Map.lookup "Index" dict of
                          Just (PDFArray idxArr) -> pairsOf idxArr
                          _                      -> [(0, size)]
      let xref = parseXRefStreamEntries w1 w2 w3 subsections streamBytes
      return (xref, dict)
    _ -> Left ("/W in XRef stream must have exactly 3 elements, got " <> show (length ws))

-- | Convert a list of PDFValues into pairs of Ints, used to decode the
-- /Index array.
pairsOf :: [PDFValue] -> [(Int, Int)]
pairsOf (PDFInt a : PDFInt b : rest) = (a, b) : pairsOf rest
pairsOf _                            = []

-- | Parse all entries from a cross-reference stream.
--
-- Per PDF spec (ISO 32000 §7.5.8.2): when a field width is 0 the field is
-- absent and defaults to its specification-defined value:
--
--   * field 1 (type): default 1 (in-use / offset entry)
--   * field 2 (byte offset or object stream number): default 0
--   * field 3 (generation number or object stream index): default 0
--
-- Fields 2 and 3 naturally default to 0 because 'readBEBytes' returns 0 for
-- a zero-width field.  Only field 1 needs an explicit check since its default
-- (1) differs from the zero returned by 'readBEBytes'.
parseXRefStreamEntries
  :: Int            -- ^ w1: width of type field (bytes); 0 means default type 1
  -> Int            -- ^ w2: width of field 2 (bytes)
  -> Int            -- ^ w3: width of field 3 (bytes)
  -> [(Int, Int)]   -- ^ subsections as (firstObj, count) pairs
  -> ByteString     -- ^ decompressed stream bytes
  -> XRef
parseXRefStreamEntries w1 w2 w3 subsections streamBytes =
  go 0 subsections IntMap.empty
  where
    entrySize = w1 + w2 + w3
    go _   []                        acc = acc
    go pos ((firstObj, count) : rest) acc =
      let newEntries =
            [ let typ = if w1 == 0 then 1 else readBEBytes w1 entryBytes
              in case typ of
                0 -> Nothing   -- free
                1 -> Just (firstObj + i,
                           XRefOffset (fromIntegral (readBEBytes w2 (BS.drop w1 entryBytes))))
                2 -> Just (firstObj + i,
                           XRefObjStm (readBEBytes w2 (BS.drop w1 entryBytes))
                                      (readBEBytes w3 (BS.drop (w1 + w2) entryBytes)))
                _ -> Nothing   -- unknown type, skip
            | i <- [0 .. count - 1]
            , let entryBytes = BS.drop (pos + i * entrySize) streamBytes
            ]
          acc' = foldl' insertEntry acc newEntries
      in go (pos + count * entrySize) rest acc'
    insertEntry m Nothing       = m
    insertEntry m (Just (k, v)) = IntMap.insert k v m

-- | Read @n@ bytes as a big-endian unsigned integer.
readBEBytes :: Int -> ByteString -> Int
readBEBytes n bs = BS.foldl' (\acc b -> acc * 256 + fromIntegral b) 0 (BS.take n bs)
