{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Conversion between PDF AcroForm fields and FDF format, replacing the need
-- for the external @pdftk@ tool.
--
-- == Usage
--
-- To extract form field data from a PDF file into an FDF value:
--
-- @
-- import qualified Data.ByteString as ByteString
-- import Text.FDF (serialize)
-- import Text.FDF.PDF (parsePDF)
--
-- main :: IO ()
-- main = do
--   pdfBytes <- ByteString.readFile "form.pdf"
--   case parsePDF pdfBytes of
--     Left err  -> putStrLn $ "Error: " ++ err
--     Right fdf -> ByteString.writeFile "form.fdf" (serialize fdf)
-- @
--
-- To fill the form fields of a PDF template using an FDF value and write the
-- resulting PDF:
--
-- @
-- import qualified Data.ByteString as ByteString
-- import Text.FDF (parse)
-- import Text.FDF.PDF (fillPDF)
--
-- main :: IO ()
-- main = do
--   fdfBytes <- ByteString.readFile "data.fdf"
--   pdfBytes <- ByteString.readFile "template.pdf"
--   case FDF.parse fdfBytes of
--     Left err  -> putStrLn $ "FDF parse error: " ++ err
--     Right fdf ->
--       case fillPDF fdf pdfBytes of
--         Left err     -> putStrLn $ "Fill error: " ++ err
--         Right filled -> ByteString.writeFile "filled.pdf" filled
-- @
module Text.FDF.PDF
  ( parsePDF
  , fillPDF
  ) where

import Control.Exception (SomeException, evaluate, try)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Builder as BB
import qualified Data.ByteString.Char8 as BSC
import qualified Data.ByteString.Lazy as LBS
import qualified Codec.Compression.Zlib as Zlib
import Data.Char (chr, intToDigit, isDigit, isHexDigit, isSpace, ord)
import Data.Int (Int64)
import Data.List (foldl')
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import Data.Word (Word8)
import System.IO.Unsafe (unsafePerformIO)

import Text.FDF (FDF (..), Field (..), FieldContent (..))
import qualified Text.FDF as FDF

-- ---------------------------------------------------------------------------
-- Public API

-- | Extract form field data from a PDF file.
--
-- Reads the PDF's AcroForm structure and returns the corresponding 'FDF'
-- value.  Supports both traditional (table-based) cross-reference sections
-- and cross-reference streams (PDF 1.5+), including FlateDecode-compressed
-- object streams.
parsePDF :: ByteString -> Either String FDF
parsePDF bs = do
  xrefOff         <- findXRefOffset bs
  (xref, trailer) <- parseXRefChain bs xrefOff
  rootRef         <- dictLookupRef "Root" trailer
  catalog         <- loadDict bs xref rootRef
  acroRef         <- dictLookupRef "AcroForm" catalog
  acroForm        <- loadDict bs xref acroRef
  fieldsArr       <- loadArray bs xref "Fields" acroForm
  fields          <- mapM (loadFieldObj bs xref) fieldsArr
  case fields of
    []  -> Left "PDF has no AcroForm fields"
    [f] -> Right $ FDF
             "1 0 obj\n"
             f
             "endobj\ntrailer\n\n<<\n/Root 1 0 R\n>>\n"
    _   -> Right $ FDF
             "1 0 obj\n"
             Field { name = "", content = Children fields }
             "endobj\ntrailer\n\n<<\n/Root 1 0 R\n>>\n"

-- | Fill the form fields of a PDF template using an 'FDF' value.
--
-- Uses an incremental-update append so the original PDF bytes are left intact.
-- Encrypted PDFs are not supported.
fillPDF :: FDF -> ByteString -> Either String ByteString
fillPDF fdf pdfBytes = do
  xrefOff         <- findXRefOffset pdfBytes
  (xref, trailer) <- parseXRefChain pdfBytes xrefOff
  rootRef         <- dictLookupRef "Root" trailer
  catalog         <- loadDict pdfBytes xref rootRef
  acroRef         <- dictLookupRef "AcroForm" catalog
  acroForm        <- loadDict pdfBytes xref acroRef
  fieldsArr       <- loadArray pdfBytes xref "Fields" acroForm
  -- Build mapping:  full path → (objNum, current dict)
  pathMap         <- buildPathMap pdfBytes xref [] fieldsArr
  -- Collect leaf-value updates from FDF
  let updates      = collectUpdates [] (body fdf)
  -- Apply updates: produce list of (objNum, new dict)
  let totalObjs    = fromMaybe 0 $ do
        PDFInt n <- Map.lookup "Size" trailer
        return n
  (newObjs, _) <- foldl' (applyUpdate pathMap) (Right ([], totalObjs)) updates
  if null newObjs
    then Right pdfBytes
    else Right $ appendIncrementalUpdate pdfBytes xrefOff trailer newObjs

-- ---------------------------------------------------------------------------
-- PDF value types

data PDFValue
  = PDFNull
  | PDFBool Bool
  | PDFInt Int
  | PDFReal Double
  | PDFName ByteString          -- without leading '/'
  | PDFString ByteString        -- decoded raw bytes (may be UTF-16BE)
  | PDFArray [PDFValue]
  | PDFDict (Map ByteString PDFValue)
  | PDFRef Int Int              -- object number, generation number
  deriving (Eq, Show)

-- ---------------------------------------------------------------------------
-- XRef

-- | A single cross-reference entry.
data XRefEntry
  = XRefOffset Int64   -- ^ byte offset of the object in the file
  | XRefObjStm Int Int -- ^ compressed: (object stream obj number, index in stream)
  deriving (Eq, Show)

-- | Cross-reference table: maps object number to its location.
type XRef = Map Int XRefEntry

-- ---------------------------------------------------------------------------
-- Finding startxref

-- | Locate the byte offset stored after the last @startxref@ keyword in the
-- file (searching the final 1 KiB).
findXRefOffset :: ByteString -> Either String Int64
findXRefOffset bs =
  let searchFrom = max 0 (BS.length bs - 1024)
      suffix     = BS.drop searchFrom bs
  in case findLast "startxref" suffix of
    Nothing  -> Left "Cannot find 'startxref' in PDF"
    Just rel ->
      let absOff  = searchFrom + rel + 9  -- skip past "startxref"
          rest    = BSC.dropWhile isSpace (BS.drop absOff bs)
      in case BSC.readInt rest of
           Just (n, _) -> Right (fromIntegral n)
           Nothing     -> Left "Cannot parse the xref offset after 'startxref'"

-- | Return the byte position of the last occurrence of @needle@ inside
-- @haystack@, or 'Nothing' if not found.
findLast :: ByteString -> ByteString -> Maybe Int
findLast needle haystack = go 0 Nothing
  where
    nLen = BS.length needle
    hLen = BS.length haystack
    go pos best
      | pos + nLen > hLen = best
      | BS.take nLen (BS.drop pos haystack) == needle = go (pos + 1) (Just pos)
      | otherwise                                     = go (pos + 1) best

-- ---------------------------------------------------------------------------
-- XRef table parsing

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
                    Right (Map.union xref stmXref)
              _ -> return xref
  -- Follow the /Prev chain to older xref sections.
  case Map.lookup "Prev" trailer of
    Just (PDFInt prev) -> do
      (prevXRef, _) <- parseXRefChain bs (fromIntegral prev)
      -- Newer (current) entries take precedence over older ones.
      return (Map.union xref' prevXRef, trailer)
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
  -- Skip "xref" + whitespace
  let bs0 = dropWS (BS.drop 4 raw)
  (bs1, xref) <- parseSubsections bs0 Map.empty
  -- bs1 should now start with "trailer"
  let afterTrailer = dropWS (BS.drop 7 bs1)  -- skip "trailer"
  case parseDict afterTrailer of
    Left err       -> Left ("Trailer dict parse error: " <> err)
    Right (td, _)  -> Right (xref, td)

-- | Parse zero or more xref subsections, stopping at "trailer".
parseSubsections :: ByteString -> XRef -> Either String (ByteString, XRef)
parseSubsections bs xref =
  -- Drop any whitespace between subsections (or before "trailer").
  let bs' = dropWS bs
  in if "trailer" `BS.isPrefixOf` bs'
       then Right (bs', xref)
       else do
         (bs'', entries) <- parseSubsection bs'
         parseSubsections bs'' (Map.union entries xref)

-- | Parse one xref subsection: @firstObj count\n@ followed by entries.
parseSubsection :: ByteString -> Either String (ByteString, XRef)
parseSubsection bs0 = do
  let (firstStr, r1) = BSC.span isDigit bs0
  when (BS.null firstStr) (Left "Expected object number in xref subsection")
  let firstObj = readDecimal firstStr
  let (countStr, r2) = BSC.span isDigit (dropWS1 r1)
  when (BS.null countStr) (Left "Expected count in xref subsection")
  let count   = readDecimal countStr
      r3      = dropLineEnd r2
      entries = Map.fromList
                  [ (firstObj + i, e)
                  | i <- [0 .. count - 1]
                  , Just e <- [parseXRefEntry (BS.take 20 (BS.drop (i * 20) r3))]
                  ]
  Right (BS.drop (count * 20) r3, entries)

-- | Parse one 20-byte xref entry.  Returns 'Nothing' for free entries.
parseXRefEntry :: ByteString -> Maybe XRefEntry
parseXRefEntry entry
  | BS.length entry >= 18 && BSC.index entry 17 == 'f' = Nothing
  | otherwise = Just (XRefOffset (fromIntegral (readDecimal (BS.take 10 entry))))

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
  go 0 subsections Map.empty
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
    insertEntry m (Just (k, v)) = Map.insert k v m

-- | Read @n@ bytes as a big-endian unsigned integer.
readBEBytes :: Int -> ByteString -> Int
readBEBytes n bs = BS.foldl' (\acc b -> acc * 256 + fromIntegral b) 0 (BS.take n bs)

-- | Convert a 'PDFValue' to an 'Int', failing with a message otherwise.
toInt :: PDFValue -> Either String Int
toInt (PDFInt n) = Right n
toInt v          = Left $ "Expected integer, got: " <> show v

-- ---------------------------------------------------------------------------
-- Stream parsing and decompression

-- | Parse an indirect stream object at the given byte offset in the file.
-- Returns the stream dictionary and the raw (possibly compressed) stream bytes.
parseStreamAt :: ByteString -> Int64 -> Either String (Map ByteString PDFValue, ByteString)
parseStreamAt bs off = do
  let chunk = dropWS (BS.drop (fromIntegral off) bs)
  -- Skip "N G obj"
  let (_, r1) = BSC.span isDigit chunk
      (_, r2) = BSC.span isDigit (dropWS r1)
      r3      = dropWS r2
  after <- case BSC.stripPrefix "obj" r3 of
    Just r  -> Right (dropWS r)
    Nothing -> Left ("Expected 'obj' at offset " <> show off)
  (dict, rest) <- parseDict after
  -- The next token should be "stream".
  let rest' = dropWS rest
  case BS.stripPrefix "stream" rest' of
    Nothing       -> Left ("Expected 'stream' keyword at offset " <> show off)
    Just afterKW  -> do
      -- Skip exactly one EOL (CR, LF, or CRLF) after the keyword.
      let streamStart = case BSC.uncons afterKW of
            Just ('\r', r) -> case BSC.uncons r of
                                Just ('\n', r') -> r'
                                _               -> r
            Just ('\n', r) -> r
            _              -> afterKW
      -- /Length must be a direct integer in xref streams; may be indirect
      -- in object streams (resolved after xref is built, see loadFromObjStream).
      len <- case Map.lookup "Length" dict of
               Just (PDFInt n) -> Right n
               Just _          -> Left "Stream /Length is not a direct integer"
               Nothing         -> Left "Stream dict missing /Length"
      Right (dict, BS.take len streamStart)

-- | Decompress stream bytes according to the /Filter and /DecodeParms entries
-- in the stream dict.  Only /FlateDecode (zlib) is currently supported as a
-- compression filter; other filters return an error.  PNG predictors
-- (Predictor 10–15, as commonly used in PDF cross-reference streams) are
-- applied after decompression.  If there is no /Filter, the bytes are
-- returned unchanged.
decompressStream :: Map ByteString PDFValue -> ByteString -> Either String ByteString
decompressStream dict rawBytes = do
  decompressed <- case Map.lookup "Filter" dict of
    Nothing                                 -> Right rawBytes
    Just (PDFName "FlateDecode")            -> zlibDecompress rawBytes
    Just (PDFArray [PDFName "FlateDecode"]) -> zlibDecompress rawBytes
    Just f -> Left ("Unsupported stream filter: " <> show f)
  applyDecodeParms dict decompressed
  where
    -- 'Zlib.decompress' operates on lazy ByteStrings and throws a
    -- 'DecompressError' exception on invalid data.  We catch it inside
    -- 'unsafePerformIO' so the rest of the module can stay in Either.
    -- This is referentially transparent: the same compressed bytes always
    -- produce the same result (or the same error).
    zlibDecompress bs = unsafePerformIO $ do
      result <- try (evaluate (LBS.toStrict (Zlib.decompress (LBS.fromStrict bs))))
      return $ case (result :: Either SomeException ByteString) of
        Right decompressed -> Right decompressed
        Left  e            -> Left ("Stream decompression error: " <> show e)

-- | Apply predictor un-filtering as specified by the @/DecodeParms@ entry in
-- @dict@.  Only PNG predictors (Predictor 10–15) are handled; other values
-- (including TIFF Predictor 2) are passed through unchanged.
--
-- PNG predictors are heavily used in PDF cross-reference streams: Adobe
-- Acrobat writes @\/FlateDecode@ streams with @\/Predictor 12@ (PNG Up),
-- which prepends a 1-byte filter-type indicator to every data row.
-- Without un-predicting the decompressed bytes, xref entries are read with
-- completely wrong type/offset values.
applyDecodeParms :: Map ByteString PDFValue -> ByteString -> Either String ByteString
applyDecodeParms dict bs =
  let params = case Map.lookup "DecodeParms" dict of
                 Just (PDFDict d)             -> d
                 Just (PDFArray (PDFDict d:_)) -> d
                 _                            -> Map.empty
  in case Map.lookup "Predictor" params of
       Just (PDFInt p) | p >= 10 ->
         let cols = case Map.lookup "Columns" params of
                      Just (PDFInt c) -> c
                      _               -> 1  -- PDF spec default
         in applyPNGFilters cols bs
       _ -> Right bs

-- | Decode PNG row filters from a decompressed byte stream.
--
-- After FlateDecode decompression, PDF streams using a PNG predictor
-- (Predictor 10–15) have each data row prefixed with a 1-byte filter-type
-- indicator.  The row width (not counting that prefix byte) is @cols@ bytes.
-- This function strips the prefix bytes and applies the corresponding inverse
-- filter to reconstruct the original row data.
applyPNGFilters :: Int -> ByteString -> Either String ByteString
applyPNGFilters cols decompressed
  | cols <= 0 = Left "PNG predictor: /Columns must be positive"
  | BS.length decompressed `mod` stride /= 0
      = Left ("PNG predictor: data length " <> show (BS.length decompressed)
              <> " not a multiple of row stride " <> show stride)
  | otherwise = Right result
  where
    stride  = cols + 1
    numRows = BS.length decompressed `div` stride
    (result, _) =
      foldl' step (BS.empty, BS.replicate cols 0) [0 .. numRows - 1]
    step (acc, prev) i =
      let pos     = i * stride
          filt    = BS.index decompressed pos
          rawRow  = BS.take cols (BS.drop (pos + 1) decompressed)
          decoded = unfilterRow filt rawRow prev
      in (acc <> decoded, decoded)

-- | Apply the inverse of a single PNG scanline filter.
-- @bpp@ (bytes per pixel) is assumed to be 1, which is always the case for
-- PDF cross-reference stream entries (single-byte components).
unfilterRow :: Word8 -> ByteString -> ByteString -> ByteString
unfilterRow filt rawRow prevRow =
  case filt of
    0 -> rawRow   -- None
    1 ->          -- Sub: decoded[i] = raw[i] + decoded[i-1]
      snd $ BS.mapAccumL subStep 0 rawRow
    2 ->          -- Up: decoded[i] = raw[i] + prev[i]
      BS.pack $ BS.zipWith addMod rawRow prevRow
    3 ->          -- Average: decoded[i] = raw[i] + floor((decoded[i-1] + prev[i]) / 2)
      snd $ foldl' avgStep (0, BS.empty) (zip [0..] (BS.unpack rawRow))
    4 ->          -- Paeth: decoded[i] = raw[i] + paeth(decoded[i-1], prev[i], prev[i-1])
      snd $ foldl' paethStep (0, BS.empty) (zip [0..] (BS.unpack rawRow))
    _ -> rawRow   -- Unknown, treat as None
  where
    subStep :: Word8 -> Word8 -> (Word8, Word8)
    subStep prev b = let d = prev + b in (d, d)  -- Word8 wraps at 256

    addMod :: Word8 -> Word8 -> Word8
    addMod r p = r + p  -- Word8 addition wraps naturally

    getPrev i
      | i < BS.length prevRow = fromIntegral (BS.index prevRow i)
      | otherwise              = 0 :: Int

    avgStep :: (Int, ByteString) -> (Int, Word8) -> (Int, ByteString)
    avgStep (prevDec, acc) (i, b) =
      let a  = prevDec
          pv = getPrev i
          d  = fromIntegral (fromIntegral b + (a + pv) `div` 2 :: Int)
      in (fromIntegral d, BS.snoc acc d)

    paethStep :: (Int, ByteString) -> (Int, Word8) -> (Int, ByteString)
    paethStep (prevDec, acc) (i, b) =
      let a  = prevDec
          bv = getPrev i
          cv = if i > 0 then getPrev (i - 1) else 0
          pr = paethPredictor a bv cv
          d  = fromIntegral (fromIntegral b + pr :: Int)
      in (fromIntegral d, BS.snoc acc d)

    paethPredictor :: Int -> Int -> Int -> Int
    paethPredictor a b c =
      let p  = a + b - c
          pa = abs (p - a)
          pb = abs (p - b)
          pc = abs (p - c)
      in if pa <= pb && pa <= pc then a
         else if pb <= pc        then b
         else                         c

-- ---------------------------------------------------------------------------
-- Object loading

-- | Load and dereference an object.  References are followed one level deep.
loadObject :: ByteString -> XRef -> PDFValue -> Either String PDFValue
loadObject bs xref (PDFRef n _) =
  case Map.lookup n xref of
    Nothing                 -> Left $ "Object " <> show n <> " not in xref"
    Just (XRefOffset off)   -> parseIndirectObject bs off
    Just (XRefObjStm sn ix) -> loadFromObjStream bs xref sn ix
loadObject _ _ v = Right v

-- | Load an object that must be a dictionary.
loadDict :: ByteString -> XRef -> (Int, Int) -> Either String (Map ByteString PDFValue)
loadDict bs xref (n, g) = do
  v <- loadObject bs xref (PDFRef n g)
  case v of
    PDFDict d -> Right d
    _         -> Left $ "Object " <> show n <> " is not a dictionary"

-- | Parse the indirect object (@N G obj ... endobj@) at the given offset.
parseIndirectObject :: ByteString -> Int64 -> Either String PDFValue
parseIndirectObject bs off = do
  let chunk = dropWS (BS.drop (fromIntegral off) bs)
  -- Skip "N G obj"
  let (_, r1) = BSC.span isDigit chunk           -- skip obj number
      (_, r2) = BSC.span isDigit (dropWS r1)     -- skip generation
      r3      = dropWS r2
  after <- case BSC.stripPrefix "obj" r3 of
    Just r  -> Right (dropWS r)
    Nothing -> Left ("Expected 'obj' at offset " <> show off)
  fst <$> parseValue after

-- | Load an object stored inside an object stream.
-- @stmObjNum@ is the object number of the ObjStm; @idx@ is the 0-based index
-- of the desired object within that stream.
loadFromObjStream :: ByteString -> XRef -> Int -> Int -> Either String PDFValue
loadFromObjStream bs xref stmObjNum idx = do
  stmOff <- case Map.lookup stmObjNum xref of
    Just (XRefOffset off) -> Right off
    Just (XRefObjStm _ _) -> Left "Object stream is itself compressed (not supported)"
    Nothing               -> Left $ "Object stream " <> show stmObjNum <> " not in xref"
  -- Parse the stream, resolving /Length via the xref if it's indirect.
  (rawDict, rawBytes) <- parseStreamAtIndirectLen bs xref stmOff
  streamBytes         <- decompressStream rawDict rawBytes
  -- /N: number of objects; /First: byte offset of first object in body.
  n     <- toInt =<< maybe (Left "ObjStm missing /N")     Right (Map.lookup "N"     rawDict)
  first <- toInt =<< maybe (Left "ObjStm missing /First") Right (Map.lookup "First" rawDict)
  -- The header is a flat list of (objNum offset) pairs.
  offsets <- parseObjStmHeader (BS.take first streamBytes) n
  (_, relOff) <-
    case drop idx offsets of
      (entry : _) -> Right entry
      []          -> Left $ "Object stream index " <> show idx <> " out of range (n=" <> show n <> ")"
  let body = BS.drop (first + relOff) streamBytes
  fst <$> parseValue (dropWS body)

-- | Like 'parseStreamAt' but resolves an indirect /Length reference.
parseStreamAtIndirectLen
  :: ByteString
  -> XRef
  -> Int64
  -> Either String (Map ByteString PDFValue, ByteString)
parseStreamAtIndirectLen bs xref off = do
  let chunk = dropWS (BS.drop (fromIntegral off) bs)
  let (_, r1) = BSC.span isDigit chunk
      (_, r2) = BSC.span isDigit (dropWS r1)
      r3      = dropWS r2
  after <- case BSC.stripPrefix "obj" r3 of
    Just r  -> Right (dropWS r)
    Nothing -> Left ("Expected 'obj' at offset " <> show off)
  (dict0, rest) <- parseDict after
  -- Resolve indirect /Length if necessary.
  dict <- case Map.lookup "Length" dict0 of
    Just (PDFRef ln lg) -> do
      lenVal <- loadObject bs xref (PDFRef ln lg)
      case lenVal of
        PDFInt n -> Right (Map.insert "Length" (PDFInt n) dict0)
        _        -> Left "Resolved /Length is not an integer"
    _ -> Right dict0
  let rest' = dropWS rest
  case BS.stripPrefix "stream" rest' of
    Nothing      -> Left ("Expected 'stream' keyword at offset " <> show off)
    Just afterKW -> do
      let streamStart = case BSC.uncons afterKW of
            Just ('\r', r) -> case BSC.uncons r of
                                Just ('\n', r') -> r'
                                _               -> r
            Just ('\n', r) -> r
            _              -> afterKW
      len <- case Map.lookup "Length" dict of
               Just (PDFInt n) -> Right n
               _               -> Left "Stream /Length missing or not resolved"
      Right (dict, BS.take len streamStart)

-- | Parse the object-number/offset header of an object stream.
-- Returns a list of @(objectNumber, relativeOffset)@ pairs.
parseObjStmHeader :: ByteString -> Int -> Either String [(Int, Int)]
parseObjStmHeader bs n = go (dropWS bs) n []
  where
    go _   0 acc = Right (reverse acc)
    go bs' k acc =
      let (numStr, r1) = BSC.span isDigit bs'
          (offStr, r2) = BSC.span isDigit (dropWS r1)
      in if BS.null numStr || BS.null offStr
         then Left "Truncated object stream header"
         else go (dropWS r2) (k - 1) ((readDecimal numStr, readDecimal offStr) : acc)

-- ---------------------------------------------------------------------------
-- AcroForm field loading

-- | Load a single field object from a PDF reference.
loadFieldObj :: ByteString -> XRef -> PDFValue -> Either String Field
loadFieldObj bs xref ref = do
  obj  <- loadObject bs xref ref
  dict <- case obj of
            PDFDict d -> Right d
            _         -> Left "Field is not a dictionary"
  buildField bs xref dict

-- | Build a 'Field' from a PDF field dictionary.
buildField :: ByteString -> XRef -> Map ByteString PDFValue -> Either String Field
buildField bs xref dict = do
  t    <- decodeFieldText dict "T"
  cont <- case Map.lookup "Kids" dict of
    Just (PDFArray kids) -> Children <$> mapM (loadFieldObj bs xref) kids
    Just ref@PDFRef{} -> do
      kidsVal <- loadObject bs xref ref
      case kidsVal of
        PDFArray kids -> Children <$> mapM (loadFieldObj bs xref) kids
        _             -> Left "Kids is not an array"
    _ -> case Map.lookup "V" dict of
      Nothing              -> Right (FieldValue "")
      Just (PDFString raw) -> Right (FieldValue (decodePDFString raw))
      Just (PDFName nm)    -> Right (FieldValue (Text.decodeLatin1 nm))
      Just _               -> Right (FieldValue "")
  return Field { name = t, content = cont }

-- | Decode the value of a string-typed field entry as 'Text'.
decodeFieldText :: Map ByteString PDFValue -> ByteString -> Either String Text
decodeFieldText dict key =
  case Map.lookup key dict of
    Nothing             -> Left ("Field is missing /" <> BSC.unpack key)
    Just (PDFString bs) -> Right (decodePDFString bs)
    Just (PDFName nm)   -> Right (Text.decodeLatin1 nm)
    Just v              -> Left ("/" <> BSC.unpack key <> " has unexpected type: " <> show v)

-- | Decode a raw PDF string (literal or hex) to 'Text'.
-- Strings starting with a UTF-16BE BOM are decoded accordingly; otherwise
-- Latin-1 (PDFDocEncoding) is assumed.
decodePDFString :: ByteString -> Text
decodePDFString bs
  | "\xFE\xFF" `BS.isPrefixOf` bs = Text.decodeUtf16BE (BS.drop 2 bs)
  | otherwise                      = Text.decodeLatin1 bs

-- ---------------------------------------------------------------------------
-- Path → object mapping (for fillPDF)

type ObjRef = (Int, Int)  -- object number, generation

-- | Build a map from field paths (encoded as slash-joined names) to the
-- corresponding (object number, generation, current dict) triple.
buildPathMap
  :: ByteString
  -> XRef
  -> [Text]    -- ^ path prefix (ancestor names)
  -> [PDFValue]
  -> Either String (Map [Text] (ObjRef, Map ByteString PDFValue))
buildPathMap bs xref prefix refs = do
  entries <- mapM (buildPathEntry bs xref prefix) refs
  return (Map.unions entries)

buildPathEntry
  :: ByteString
  -> XRef
  -> [Text]
  -> PDFValue
  -> Either String (Map [Text] (ObjRef, Map ByteString PDFValue))
buildPathEntry bs xref prefix ref = do
  (objNum, objGen) <- case ref of
    PDFRef n g -> Right (n, g)
    _          -> Left "Field entry in /Fields is not a reference"
  dict <- loadDict bs xref (objNum, objGen)
  t    <- decodeFieldText dict "T"
  let path = prefix ++ [t]
  case Map.lookup "Kids" dict of
    Just (PDFArray kids) ->
      buildPathMap bs xref path kids
    Just r@PDFRef{} -> do
      kidsVal <- loadObject bs xref r
      case kidsVal of
        PDFArray kids -> buildPathMap bs xref path kids
        _             -> Left "Kids is not an array"
    _ ->
      -- This is a leaf field.
      Right $ Map.singleton path ((objNum, objGen), dict)

-- ---------------------------------------------------------------------------
-- Collecting FDF leaf values

-- | Return all leaf (path, value) pairs from an FDF body.
collectUpdates :: [Text] -> Field -> [([Text], Text)]
collectUpdates prefix Field { name = n, content = cont } =
  let path = if Text.null n then prefix else prefix ++ [n]
  in case cont of
    FieldValue v  -> [(path, v)]
    Children kids -> concatMap (collectUpdates path) kids

-- ---------------------------------------------------------------------------
-- Applying updates

type UpdateAcc = Either String ([(ObjRef, Map ByteString PDFValue)], Int)

-- | Add a modified field entry for one leaf-value update.
applyUpdate
  :: Map [Text] (ObjRef, Map ByteString PDFValue)
  -> UpdateAcc
  -> ([Text], Text)
  -> UpdateAcc
applyUpdate _pathMap (Left err) _ = Left err
applyUpdate pathMap (Right (objs, maxN)) (path, newVal) =
  case Map.lookup path pathMap of
    Nothing        -> Right (objs, maxN)  -- field not in PDF, skip
    Just (ref, d)  ->
      let newDict = Map.insert "V" (PDFString (encodePDFStringValue newVal)) d
      in Right ((ref, newDict) : objs, maxN)

-- ---------------------------------------------------------------------------
-- Incremental update writer

-- | Append new object versions and an updated xref/trailer to @pdfBytes@.
appendIncrementalUpdate
  :: ByteString
  -> Int64                                           -- previous xref offset
  -> Map ByteString PDFValue                         -- original trailer
  -> [(ObjRef, Map ByteString PDFValue)]             -- updated objects
  -> ByteString
appendIncrementalUpdate pdfBytes prevXrefOff origTrailer updatedObjs =
  let baseLen  = fromIntegral (BS.length pdfBytes)
      -- Serialize each updated object and record its new offset.
      (objBlocks, offsets) = buildObjBlocks baseLen updatedObjs
      -- Build new xref section.
      newXrefOff           = baseLen + fromIntegral (LBS.length (BB.toLazyByteString (mconcat objBlocks)))
      newXref              = buildXRefSection offsets
      -- Build new trailer.
      origSize = fromMaybe 0 $ do
                   PDFInt n <- Map.lookup "Size" origTrailer
                   return n
      newSize  = origSize   -- we reuse old object numbers
      newTrailer = buildTrailerSection newSize prevXrefOff origTrailer
  in LBS.toStrict $ BB.toLazyByteString $
       BB.byteString pdfBytes
       <> mconcat objBlocks
       <> BB.byteString newXref
       <> BB.byteString newTrailer
       <> "startxref\n"
       <> BB.int64Dec newXrefOff <> "\n"
       <> "%%EOF\n"

-- | Serialize updated objects and return (BB blocks, (objRef, offset) list).
buildObjBlocks
  :: Int64
  -> [(ObjRef, Map ByteString PDFValue)]
  -> ([BB.Builder], [(ObjRef, Int64)])
buildObjBlocks startOff objs =
  let go off [] = ([], [])
      go off ((ref@(n,g), dict) : rest) =
        let block   = serializeObj n g dict
            blockBS = LBS.toStrict (BB.toLazyByteString block)
            len     = fromIntegral (BS.length blockBS)
            (blocks, offsets) = go (off + len) rest
        in (block : blocks, (ref, off) : offsets)
  in go startOff objs

serializeObj :: Int -> Int -> Map ByteString PDFValue -> BB.Builder
serializeObj n g dict =
     BB.intDec n <> " " <> BB.intDec g <> " obj\n"
  <> serializeDict dict <> "\n"
  <> "endobj\n"

serializeDict :: Map ByteString PDFValue -> BB.Builder
serializeDict d =
  "<<\n" <> Map.foldlWithKey' go mempty d <> ">>"
  where
    go acc k v =
      acc <> "/" <> BB.byteString k <> " " <> serializeValue v <> "\n"

serializeValue :: PDFValue -> BB.Builder
serializeValue = \case
  PDFNull         -> "null"
  PDFBool True    -> "true"
  PDFBool False   -> "false"
  PDFInt n        -> BB.intDec n
  PDFReal r       -> BB.string7 (show r)
  PDFName nm      -> "/" <> BB.byteString nm
  PDFString bs    -> serializePDFString bs
  PDFArray vs     -> "[" <> foldMap (\v -> serializeValue v <> " ") vs <> "]"
  PDFDict d       -> serializeDict d
  PDFRef n g      -> BB.intDec n <> " " <> BB.intDec g <> " R"

-- | Serialize a raw string value using parentheses notation (ASCII) or
-- angle-bracket hex notation (non-ASCII / binary).
serializePDFString :: ByteString -> BB.Builder
serializePDFString bs
  | isAsciiSafe bs = "(" <> BB.byteString (escapeLiteral bs) <> ")"
  | otherwise      = "<" <> BB.byteString (hexEncode bs) <> ">"
  where
    isAsciiSafe = BS.all (\w -> w >= 0x20 && w <= 0x7E
                              && w /= 0x28 && w /= 0x29 && w /= 0x5C)

-- | Escape special characters inside a PDF literal string.
escapeLiteral :: ByteString -> ByteString
escapeLiteral = BS.concatMap escape
  where
    escape 0x28 = "\\("
    escape 0x29 = "\\)"
    escape 0x5C = "\\\\"
    escape w    = BS.singleton w

-- | Encode a 'Text' value as a raw PDF string, using UTF-16BE for non-ASCII.
encodePDFStringValue :: Text -> ByteString
encodePDFStringValue t
  | Text.all isAsciiPrintable t = Text.encodeUtf8 t
  | otherwise                   = "\xFE\xFF" <> Text.encodeUtf16BE t
  where
    isAsciiPrintable c = c >= ' ' && c <= '~'

hexEncode :: ByteString -> ByteString
hexEncode = BS.concatMap
  (\w -> BSC.pack [intToDigit (fromIntegral (w `div` 16)),
                   intToDigit (fromIntegral (w `mod` 16))])

-- | Build the xref section for the incremental update.
-- Groups updated objects into consecutive subsections under a single
-- @xref@ keyword, as required by the PDF specification.
buildXRefSection :: [(ObjRef, Int64)] -> ByteString
buildXRefSection [] = ""
buildXRefSection offsets =
  let entries = map (\((n, _), off) -> (n, off)) offsets
      sorted  = Map.toAscList $
                  foldl' (\m (n, off) -> Map.insert n off m) Map.empty entries
  in BSC.pack $ "xref\n" <> concatMap renderSubsection (groupConsecutive sorted)
  where
    renderSubsection [] = ""
    renderSubsection grp@((n, _) : _) =
      show n <> " " <> show (length grp) <> "\n"
        <> concatMap (\(_, off) -> padDec10 off <> " 00000 n\r\n") grp

    groupConsecutive :: [(Int, Int64)] -> [[(Int, Int64)]]
    groupConsecutive [] = []
    groupConsecutive [x] = [[x]]
    groupConsecutive (x@(n1, _) : rest@((n2, _) : _))
      | n2 == n1 + 1 = case groupConsecutive rest of
                         []          -> [[x]]
                         (grp : grps) -> (x : grp) : grps
      | otherwise    = [x] : groupConsecutive rest

padDec10 :: Int64 -> String
padDec10 n = let s = show n in replicate (10 - length s) '0' <> s

-- | Build the trailer section (without @startxref@ line).
buildTrailerSection
  :: Int
  -> Int64
  -> Map ByteString PDFValue
  -> ByteString
buildTrailerSection size prevOff origTrailer =
  let td = Map.fromList $
              [ ("Size", PDFInt size)
              , ("Prev", PDFInt (fromIntegral prevOff))
              ] <>
              -- Carry over /Root and /Info from the original trailer.
              [ (k, v)
              | k <- ["Root", "Info", "Encrypt"]
              , Just v <- [Map.lookup k origTrailer]
              ]
  in LBS.toStrict $ BB.toLazyByteString $
       "trailer\n" <> serializeDict td <> "\n"

-- ---------------------------------------------------------------------------
-- PDF value parser
--
-- All parsing is purely positional: each @parse*@ function takes a
-- 'ByteString' starting at the current position and returns the parsed value
-- together with the remaining input.

type ParseResult a = Either String (a, ByteString)

parseValue :: ByteString -> ParseResult PDFValue
parseValue bs0 =
  let bs = dropWS bs0
  in if BS.null bs
     then Left "Unexpected end of input"
     else case BSC.head bs of
       'n' | "null"  `BS.isPrefixOf` bs -> Right (PDFNull,        BS.drop 4 bs)
       't' | "true"  `BS.isPrefixOf` bs -> Right (PDFBool True,   BS.drop 4 bs)
       'f' | "false" `BS.isPrefixOf` bs -> Right (PDFBool False,  BS.drop 5 bs)
       '/'  -> (\(nm, r) -> (PDFName nm, r)) <$> parseName (BS.tail bs)
       '('  -> (\(s,  r) -> (PDFString s, r)) <$> parseLiteralString (BS.tail bs) 0
       '<'  ->
         if BS.length bs >= 2 && BSC.index bs 1 == '<'
           then (\(d, r) -> (PDFDict d, r)) <$> parseDict bs
           else (\(s, r) -> (PDFString s, r)) <$> parseHexString (BS.drop 1 bs)
       '['  -> (\(a, r) -> (PDFArray a, r)) <$> parseArray (BS.drop 1 bs)
       c | c == '-' || c == '+' || isDigit c -> parseNumOrRef bs
       _    -> Left ("Unexpected character: " <> [BSC.head bs])

-- | Parse a PDF name token (without the leading '/').
parseName :: ByteString -> ParseResult ByteString
parseName bs =
  let (nm, rest) = BS.span isNameByte bs
  in if BS.null nm
     then Left "Empty name"
     else Right (nm, rest)
  where
    isNameByte w =
      let c = chr (fromIntegral w)
      in not (isSpace c) && c `notElem` ("/()<>[]{}%\0" :: String)

-- | Parse a PDF literal string (the opening '(' has already been consumed).
-- Depth tracks nesting: each '(' increments it and each ')' decrements it;
-- at depth zero a ')' ends the string.
parseLiteralString :: ByteString -> Int -> ParseResult ByteString
parseLiteralString bs0 depth0 = go bs0 depth0 mempty
  where
    go bs depth acc
      | BS.null bs = Left "Unterminated literal string"
      | otherwise  =
          let (w, rest) = (BS.head bs, BS.tail bs)
              c = chr (fromIntegral w)
          in case c of
               ')' | depth == 0 -> Right (acc, rest)
               ')'  -> go rest (depth - 1) (BS.snoc acc w)
               '('  -> go rest (depth + 1) (BS.snoc acc w)
               '\\' -> do
                 (esc, rest') <- parseLiteralEscape rest
                 go rest' depth (acc <> esc)
               _ -> go rest depth (BS.snoc acc w)

parseLiteralEscape :: ByteString -> ParseResult ByteString
parseLiteralEscape bs
  | BS.null bs = Left "Unterminated escape in literal string"
  | otherwise  =
      let (w, rest) = (BS.head bs, BS.tail bs)
          c = chr (fromIntegral w)
      in case c of
           'n'  -> Right ("\n", rest)
           'r'  -> Right ("\r", rest)
           't'  -> Right ("\t", rest)
           'b'  -> Right ("\b", rest)
           'f'  -> Right ("\f", rest)
           '('  -> Right ("(",  rest)
           ')'  -> Right (")",  rest)
           '\\' -> Right ("\\", rest)
           '\r' ->  -- line continuation
             let rest' = case BSC.uncons rest of
                           Just ('\n', r) -> r
                           _              -> rest
             in Right ("", rest')
           '\n' -> Right ("", rest)  -- line continuation
           d | d >= '0' && d <= '7' ->
             -- Read 1–3 octal digits (first digit already in hand as w).
             let (extra, _) = BS.span isOctByte (BS.take 2 rest)
                 -- Combine the first digit with up to 2 more.
                 allDigits  = BS.cons w extra
                 val        = BS.foldl' (\acc b -> acc * 8 + fromIntegral b - 0x30) 0 allDigits
             in Right (BS.singleton val, BS.drop (BS.length extra) rest)
           _ -> Right (BS.singleton w, rest)
  where
    isOctByte b = b >= 0x30 && b <= 0x37

-- | Parse a PDF hex string (the opening '<' has already been consumed).
parseHexString :: ByteString -> ParseResult ByteString
parseHexString bs0 = go bs0 []
  where
    go bs acc =
      let bs' = BSC.dropWhile isSpace bs
      in case BSC.uncons bs' of
           Nothing        -> Left "Unterminated hex string"
           Just ('>', r)  -> Right (BS.pack (reverse acc), r)
           Just (h1, r1) | isHexDigit h1 ->
             let (h2c, r2) = case BSC.uncons (BSC.dropWhile isSpace r1) of
                               Just (h, r) | isHexDigit h -> (h, r)
                               _                           -> ('0', BSC.dropWhile isSpace r1)
                 val = fromIntegral (hexDigit h1 * 16 + hexDigit h2c)
             in go r2 (val : acc)
           Just (c, _) -> Left ("Invalid hex digit: " <> [c])

hexDigit :: Char -> Int
hexDigit c
  | c >= '0' && c <= '9' = ord c - ord '0'
  | c >= 'a' && c <= 'f' = ord c - ord 'a' + 10
  | c >= 'A' && c <= 'F' = ord c - ord 'A' + 10
  | otherwise             = 0

-- | Parse a PDF array (the opening '[' has already been consumed).
parseArray :: ByteString -> ParseResult [PDFValue]
parseArray bs0 = go (dropWS bs0) []
  where
    go bs acc
      | BS.null bs         = Left "Unterminated array"
      | BSC.head bs == ']' = Right (reverse acc, BS.tail bs)
      | otherwise          = do
          (v, rest) <- parseValue bs
          go (dropWS rest) (v : acc)

-- | Parse a PDF dictionary (@\<\< ... \>\>@).
parseDict :: ByteString -> ParseResult (Map ByteString PDFValue)
parseDict bs0 = do
  rest0 <- case BS.stripPrefix "<<" bs0 of
    Just r  -> Right r
    Nothing -> Left ("Expected '<<', got: " <> BSC.unpack (BS.take 10 bs0))
  go (dropWS rest0) Map.empty
  where
    go bs acc
      | BS.null bs                      = Left "Unterminated dictionary"
      | ">>" `BS.isPrefixOf` bs         = Right (acc, BS.drop 2 bs)
      | BSC.head bs == '/'              = do
          (nm, r1) <- parseName (BS.tail bs)
          (v,  r2) <- parseValue (dropWS r1)
          go (dropWS r2) (Map.insert nm v acc)
      | otherwise                       =
          Left ("Unexpected char in dictionary: " <> [BSC.head bs])

-- | Parse an integer, real, or indirect reference (e.g. @1 0 R@).
parseNumOrRef :: ByteString -> ParseResult PDFValue
parseNumOrRef bs0 =
  let (sign, bs1) = case BSC.uncons bs0 of
                      Just ('-', r) -> ("-", r)
                      Just ('+', r) -> ("",  r)
                      _             -> ("",  bs0)
      (digits, rest) = BSC.span isDigit bs1
  in if BS.null digits
     then Left ("Expected number, got: " <> BSC.unpack (BS.take 10 bs0))
     else
       let n = readDecimal digits
           signedN = if sign == "-" then negate n else n
           -- Peek ahead: might be a real number or indirect reference.
           rest' = dropWS rest
       in case BSC.uncons rest' of
            Just ('.', afterDot) ->
              let (frac, rest'') = BSC.span isDigit afterDot
                  dVal = fromIntegral signedN + fracVal frac
              in Right (PDFReal dVal, rest'')
            Just (c, _) | isDigit c && sign == "" ->
              -- Could be "N G R" (indirect reference).
              let (gen, rest'') = BSC.span isDigit rest'
                  rest''' = dropWS rest''
              in case BS.stripPrefix "R" rest''' of
                   Just r  -> Right (PDFRef n (readDecimal gen), dropWS r)
                   Nothing -> Right (PDFInt signedN, rest')
            _ -> Right (PDFInt signedN, rest')
  where
    fracVal bs = fromIntegral (readDecimal bs) / (10 ^ BS.length bs)

-- ---------------------------------------------------------------------------
-- Dictionary lookup helpers

dictLookupRef :: ByteString -> Map ByteString PDFValue -> Either String (Int, Int)
dictLookupRef key d =
  case Map.lookup key d of
    Just (PDFRef n g) -> Right (n, g)
    Just _            -> Left ("/" <> BSC.unpack key <> " is not a reference")
    Nothing           -> Left ("/" <> BSC.unpack key <> " not found in dict")

-- | Look up an array value in a dictionary, following an indirect reference
-- if needed.
loadArray
  :: ByteString
  -> XRef
  -> ByteString                    -- ^ key name
  -> Map ByteString PDFValue
  -> Either String [PDFValue]
loadArray bs xref key d =
  case Map.lookup key d of
    Just (PDFArray a) -> Right a
    Just ref@PDFRef{} -> do
      v <- loadObject bs xref ref
      case v of
        PDFArray a -> Right a
        _          -> Left ("/" <> BSC.unpack key <> " reference is not an array")
    Just _ -> Left ("/" <> BSC.unpack key <> " is not an array")
    Nothing -> Left ("/" <> BSC.unpack key <> " not found in dict")

-- ---------------------------------------------------------------------------
-- Whitespace / utility helpers

-- | Drop PDF whitespace (space, tab, CR, LF, FF, NUL) from the front.
dropWS :: ByteString -> ByteString
dropWS = BSC.dropWhile isPDFWS

-- | Drop exactly one space or nothing (used between xref header tokens).
dropWS1 :: ByteString -> ByteString
dropWS1 bs = case BSC.uncons bs of
  Just (c, r) | isPDFWS c -> r
  _                        -> bs

isPDFWS :: Char -> Bool
isPDFWS c = c `elem` (" \t\r\n\f\0" :: String)

-- | Drop a line ending (CR, LF, or CRLF) from the front.
dropLineEnd :: ByteString -> ByteString
dropLineEnd bs = case BSC.uncons bs of
  Just (' ',  r) -> dropLineEnd r
  Just ('\r', r) -> case BSC.uncons r of
                      Just ('\n', r') -> r'
                      _               -> r
  Just ('\n', r) -> r
  _              -> bs

-- | Read a non-negative decimal integer from a 'ByteString'.
readDecimal :: ByteString -> Int
readDecimal bs = fst $ fromMaybe (0, "") (BSC.readInt bs)

-- | Convenience guard that short-circuits with a 'Left' message.
when :: Bool -> Either String () -> Either String ()
when True  e = e
when False _ = Right ()
