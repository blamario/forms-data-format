{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Conversion between PDF AcroForm fields and FDF format.
--
-- References: PDF 32000-1:2008 (PDF 1.7 specification):
--
-- * §7.3 – Objects (booleans, numbers, strings, names, arrays, dictionaries)
-- * §7.5 – File structure (cross-reference tables and streams, incremental updates)
-- * §7.6 – Encryption (Standard Security Handler, AES-128-CBC, per-object keys)
-- * §12.7 – Interactive forms (AcroForm, field dictionaries, @\/Kids@, @\/T@, @\/V@)
--
-- The module is intentionally self-contained (no external PDF library dependency).

module Text.FDF.PDF
  ( parsePDF
  , fillPDF
  ) where

import Control.Exception (SomeException, evaluate, try)
import Control.Monad (when)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Builder as BB
import qualified Data.ByteString.Char8 as BSC
import qualified Data.ByteString.Lazy as LBS
import qualified Codec.Compression.Zlib as Zlib
import Data.Char (intToDigit, isDigit, isSpace)
import Numeric (showFFloat)
import Data.Int (Int64)
import qualified Data.IntMap.Strict as IntMap
import Data.List (foldl')
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NonEmpty
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (catMaybes, fromMaybe)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import Data.Word (Word8)
import System.IO.Unsafe (unsafePerformIO)

import Text.FDF (FDF (..), Field (..), FieldContent (..))
import Text.FDF.PDF.Decrypt (Decryptor, Encryptor, buildDecryptor)
import Text.FDF.PDF.Parse
import Text.FDF.PDF.Types

-- | Extract form field data from a PDF file.
--
-- Reads the PDF's AcroForm structure and returns the corresponding 'FDF'
-- value.  Supports both traditional (table-based) cross-reference sections
-- and cross-reference streams (PDF 1.5+), including FlateDecode-compressed
-- object streams.
parsePDF :: ByteString -> Either String FDF
parsePDF bs = do
  (_, xref, trailer, dec, _enc, fieldsArr) <- loadAcroFormFields bs
  fields <- catMaybes <$> mapM (loadFieldObj bs xref dec) fieldsArr
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
-- For encrypted PDFs (Standard Security Handler, empty user password), the
-- new field-value objects are AES-encrypted with the same key as the original
-- body; the incremental update trailer carries both @\/Encrypt@ and @\/ID@ so
-- readers can decrypt both the original body objects and the new field objects.
fillPDF :: FDF -> ByteString -> Either String ByteString
fillPDF fdf pdfBytes = do
  (xrefOff, xref, trailer, dec, enc, fieldsArr) <- loadAcroFormFields pdfBytes
  -- Build mapping:  full path → (objNum, current dict)
  pathMap      <- buildPathMap pdfBytes xref dec [] fieldsArr
  -- Collect leaf-value updates from FDF
  let updates   = collectUpdates [] (body fdf)
  -- Apply updates: produce list of (objNum, new dict)
  let totalObjs = fromMaybe 0 $ do
        PDFInt n <- Map.lookup "Size" trailer
        return n
  (newObjs, _) <- foldl' (applyUpdate pathMap) (Right ([], totalObjs)) updates
  Right $ if null newObjs
    then pdfBytes
    else appendIncrementalUpdate enc pdfBytes xrefOff trailer newObjs

-- | Parse the PDF cross-reference tables and locate the AcroForm @\/Fields@
-- array, returning the xref offset, xref table, trailer dictionary,
-- decryptor, encryptor, and field references.
loadAcroFormFields
  :: ByteString
  -> Either String (Int64, XRef, Map ByteString PDFValue, Decryptor, Encryptor, [PDFValue])
loadAcroFormFields bs = do
  xrefOff         <- findXRefOffset bs
  (xref, trailer) <- parseXRefChain bs xrefOff
  (dec, enc)      <- buildDecryptor bs xref trailer
  rootRef         <- dictLookupRef "Root" trailer
  catalog         <- loadDict bs xref dec rootRef
  acroRef         <- dictLookupRef "AcroForm" catalog
  acroForm        <- loadDict bs xref dec acroRef
  fieldsArr       <- loadArray bs xref dec "Fields" acroForm
  return (xrefOff, xref, trailer, dec, enc, fieldsArr)

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
  -- Skip "xref" + whitespace
  let bs0 = dropWS (BS.drop 4 raw)
  (bs1, xref) <- parseSubsections bs0 IntMap.empty
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
         parseSubsections bs'' (IntMap.union entries xref)

-- | Parse one xref subsection: @firstObj count\n@ followed by entries.
parseSubsection :: ByteString -> Either String (ByteString, XRef)
parseSubsection bs0 = do
  let (firstStr, r1) = BSC.span isDigit bs0
  when (BS.null firstStr) $ Left "Expected object number in xref subsection"
  firstObj <- readDecimal firstStr
  let (countStr, r2) = BSC.span isDigit (dropWS1 r1)
  when (BS.null countStr) $ Left "Expected count in xref subsection"
  count <- readDecimal countStr
  let r3      = dropLineEnd r2
      entries = IntMap.fromList
                  [ (firstObj + i, e)
                  | i <- [0 .. count - 1]
                  , Just e <- [parseXRefEntry (BS.take 20 (BS.drop (i * 20) r3))]
                  ]
  Right (BS.drop (count * 20) r3, entries)

-- | Parse one 20-byte xref entry.  Returns 'Nothing' for free entries.
parseXRefEntry :: ByteString -> Maybe XRefEntry
parseXRefEntry entry
  | BS.length entry >= 18 && BSC.index entry 17 == 'f' = Nothing
  | otherwise = case BSC.readInt (BS.take 10 entry) of
      Just (n, _) -> Just (XRefOffset (fromIntegral n))
      Nothing     -> Nothing  -- malformed entry, skip

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
-- For objects at a byte offset (@XRefOffset@) in an encrypted PDF, all
-- 'PDFString' leaf values in the result are decrypted with @dec@ using the
-- per-object key (objNum, genNum).  Objects within an object stream
-- (@XRefObjStm@) do not need per-string decryption because the stream itself
-- is already decrypted before parsing (per PDF spec §7.6.5).
loadObject :: ByteString -> XRef -> Decryptor -> PDFValue -> Either String PDFValue
loadObject bs xref dec (PDFRef n g) =
  case IntMap.lookup n xref of
    Nothing                 -> Left $ "Object " <> show n <> " not in xref"
    Just (XRefOffset off)   -> parseIndirectObject bs off >>= decryptPDFValue dec n g
    Just (XRefObjStm sn ix) -> loadFromObjStream bs xref dec sn ix
loadObject _ _ _ v = Right v

-- | Recursively decrypt all 'PDFString' leaf values in a 'PDFValue' using the
-- supplied 'Decryptor' with the given per-object (@objNum@, @genNum@) key.
-- 'noDecrypt' makes this a no-op for unencrypted PDFs.
decryptPDFValue :: Decryptor -> Int -> Int -> PDFValue -> Either String PDFValue
decryptPDFValue dec n g (PDFString bs) = PDFString <$> dec n g bs
decryptPDFValue dec n g (PDFArray vs)  = PDFArray  <$> mapM (decryptPDFValue dec n g) vs
decryptPDFValue dec n g (PDFDict d)    = PDFDict   <$> mapM (decryptPDFValue dec n g) d
decryptPDFValue _   _ _ v              = Right v

-- | Encrypt all 'PDFString' leaf values in a dictionary using the given
-- 'Encryptor' (for a specific object number and generation number).
-- 'PDFName', 'PDFRef', and other non-string values are left unchanged.
encryptPDFValues :: Encryptor -> Int -> Int -> Map ByteString PDFValue -> Map ByteString PDFValue
encryptPDFValues enc n g = Map.map go
  where
    go (PDFString bs) = PDFString (enc n g bs)
    go (PDFArray vs)  = PDFArray  (map go vs)
    go (PDFDict d)    = PDFDict   (Map.map go d)
    go v              = v

-- | Load an object that must be a dictionary.
loadDict :: ByteString -> XRef -> Decryptor -> (Int, Int) -> Either String (Map ByteString PDFValue)
loadDict bs xref dec (n, g) = do
  v <- loadObject bs xref dec (PDFRef n g)
  case v of
    PDFDict d -> Right d
    _         -> Left $ "Object " <> show n <> " is not a dictionary"

-- | Load an object stored inside an object stream.
-- @stmObjNum@ is the object number of the ObjStm; @idx@ is the 0-based index
-- of the desired object within that stream.
loadFromObjStream :: ByteString -> XRef -> Decryptor -> Int -> Int -> Either String PDFValue
loadFromObjStream bs xref dec stmObjNum idx = do
  stmOff <- case IntMap.lookup stmObjNum xref of
    Just (XRefOffset off) -> Right off
    Just (XRefObjStm _ _) -> Left "Object stream is itself compressed (not supported)"
    Nothing               -> Left $ "Object stream " <> show stmObjNum <> " not in xref"
  -- Parse the stream, resolving /Length via the xref if it's indirect.
  (rawDict, rawBytes) <- parseStreamAtIndirectLen bs xref dec stmObjNum stmOff
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

-- | Like 'parseStreamAt' but resolves an indirect @\/Length@ reference and
-- applies the decryptor before decompression.
parseStreamAtIndirectLen
  :: ByteString
  -> XRef
  -> Decryptor
  -> Int    -- ^ object number of the stream (for per-object decryption key)
  -> Int64
  -> Either String (Map ByteString PDFValue, ByteString)
parseStreamAtIndirectLen bs xref dec objNum off = do
  let chunk = dropWS (BS.drop (fromIntegral off) bs)
  let (_, r1) = BSC.span isDigit chunk
      (genStr, r2) = BSC.span isDigit (dropWS r1)
      r3      = dropWS r2
  genNum <- readDecimal genStr
  after <- case BSC.stripPrefix "obj" r3 of
    Just r  -> Right (dropWS r)
    Nothing -> Left ("Expected 'obj' at offset " <> show off)
  (dict0, rest) <- parseDict after
  -- Resolve indirect /Length if necessary.
  dict <- case Map.lookup "Length" dict0 of
    Just (PDFRef ln lg) -> do
      lenVal <- loadObject bs xref dec (PDFRef ln lg)
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
      let rawBytes = BS.take len streamStart
      -- Decrypt before decompression (xref streams are not encrypted;
      -- ObjStm and other streams are encrypted when /Encrypt is present).
      decrypted <- dec objNum genNum rawBytes
      Right (dict, decrypted)

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
         else do
           n <- readDecimal numStr
           o <- readDecimal offStr
           go (dropWS r2) (k - 1) ((n, o) : acc)

-- ---------------------------------------------------------------------------
-- AcroForm field loading

-- | Load a single field object from a PDF reference.
loadFieldObj :: ByteString -> XRef -> Decryptor -> PDFValue -> Either String (Maybe Field)
loadFieldObj bs xref dec ref = do
  obj  <- loadObject bs xref dec ref
  dict <- case obj of
            PDFDict d -> Right d
            _         -> Left "Field is not a dictionary"
  buildField bs xref dec dict

-- | Build a 'Field' from a PDF field dictionary.
-- Returns 'Nothing' for widget annotations that have no @\/T@ entry and so
-- cannot participate in the FDF field hierarchy.
buildField :: ByteString -> XRef -> Decryptor -> Map ByteString PDFValue -> Either String (Maybe Field)
buildField bs xref dec dict =
  case Map.lookup "T" dict of
    Nothing -> Right Nothing  -- widget annotation without /T; skip it
    _ -> do
      t    <- decodeFieldText dict "T"
      cont <- case Map.lookup "Kids" dict of
        Just (PDFArray kids) -> kidsContent bs xref dec dict kids
        Just ref@PDFRef{} -> do
          kidsVal <- loadObject bs xref dec ref
          case kidsVal of
            PDFArray kids -> kidsContent bs xref dec dict kids
            _             -> Left "Kids is not an array"
        _ -> leafFieldValue dict
      return $ Just Field { name = t, content = cont }

-- | Build the 'FieldContent' for a field's @\/Kids@ array.
-- If all kids are anonymous widget annotations (no @\/T@), the field is
-- treated as a leaf and its own @\/V@ value is returned instead.
kidsContent
  :: ByteString -> XRef -> Decryptor
  -> Map ByteString PDFValue  -- ^ the parent field's dictionary (for fallback @\/V@)
  -> [PDFValue]               -- ^ the raw kid references
  -> Either String FieldContent
kidsContent bs xref dec parentDict kids = do
  namedKids <- catMaybes <$> mapM (loadFieldObj bs xref dec) kids
  if null namedKids
    then leafFieldValue parentDict   -- all kids are widget annotations → leaf
    else return (Children namedKids)

-- | Extract the @\/V@ field value from a PDF field dictionary as a 'FieldContent'.
-- PDF name objects (including empty ones, representing no-selection) are returned
-- as 'FieldNameValue'; string objects are returned as 'FieldValue'.
leafFieldValue :: Map ByteString PDFValue -> Either String FieldContent
leafFieldValue dict = case Map.lookup "V" dict of
  Nothing              -> Right (FieldValue "")
  Just (PDFString raw) -> Right (FieldValue (decodePDFString raw))
  Just (PDFName nm)    -> Right (FieldNameValue (Text.decodeLatin1 nm))
  Just _               -> Right (FieldValue "")

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
  -> Decryptor
  -> [Text]    -- ^ path prefix (ancestor names)
  -> [PDFValue]
  -> Either String (Map (NonEmpty Text) (ObjRef, Map ByteString PDFValue))
buildPathMap bs xref dec prefix refs = do
  entries <- mapM (buildPathEntry bs xref dec prefix) refs
  return (Map.unions entries)

buildPathEntry
  :: ByteString
  -> XRef
  -> Decryptor
  -> [Text]
  -> PDFValue
  -> Either String (Map (NonEmpty Text) (ObjRef, Map ByteString PDFValue))
buildPathEntry bs xref dec prefix ref = do
  (objNum, objGen) <- case ref of
    PDFRef n g -> Right (n, g)
    _          -> Left "Field entry in /Fields is not a reference"
  dict <- loadDict bs xref dec (objNum, objGen)
  case Map.lookup "T" dict of
    Nothing -> Right Map.empty  -- widget annotation without /T; skip it
    _ -> do
      t <- decodeFieldText dict "T"
      let path   = prefix ++ [t]
          pathNE = NonEmpty.fromList path  -- safe: appending [t] ensures non-empty
      case Map.lookup "Kids" dict of
        Just (PDFArray kids) -> do
          m <- buildPathMap bs xref dec path kids
          if Map.null m
            then Right $ Map.singleton pathNE ((objNum, objGen), dict)  -- all kids are widgets
            else Right m
        Just r@PDFRef{} -> do
          kidsVal <- loadObject bs xref dec r
          case kidsVal of
            PDFArray kids -> do
              m <- buildPathMap bs xref dec path kids
              if Map.null m
                then Right $ Map.singleton pathNE ((objNum, objGen), dict)
                else Right m
            _ -> Left "Kids is not an array"
        _ ->
          -- This is a leaf field.
          Right $ Map.singleton pathNE ((objNum, objGen), dict)

-- ---------------------------------------------------------------------------
-- Collecting FDF leaf values

-- | Return all leaf (path, value) pairs from an FDF body.
collectUpdates :: [Text] -> Field -> [(NonEmpty Text, Text)]
collectUpdates prefix Field { name = n, content = cont } =
  let path = if Text.null n then prefix else prefix ++ [n]
  in case cont of
    FieldValue v     -> case NonEmpty.nonEmpty path of
                          Nothing     -> []
                          Just pathNE -> [(pathNE, v)]
    FieldNameValue v -> case NonEmpty.nonEmpty path of
                          Nothing     -> []
                          Just pathNE -> [(pathNE, v)]
    Children kids    -> concatMap (collectUpdates path) kids

-- ---------------------------------------------------------------------------
-- Applying updates

type UpdateAcc = Either String ([(ObjRef, Map ByteString PDFValue)], Int)

-- | Add a modified field entry for one leaf-value update.
applyUpdate
  :: Map (NonEmpty Text) (ObjRef, Map ByteString PDFValue)
  -> UpdateAcc
  -> (NonEmpty Text, Text)
  -> UpdateAcc
applyUpdate _pathMap (Left err) _ = Left err
applyUpdate pathMap (Right (objs, maxN)) (path, newVal) =
  case Map.lookup path pathMap of
    Nothing        -> Right (objs, maxN)  -- field not in PDF, skip
    Just (ref, d)  ->
      -- Preserve the original /V type: name fields stay as PDFName, strings as PDFString.
      let pdfVal = case Map.lookup "V" d of
                     Just (PDFName _) -> PDFName (Text.encodeUtf8 newVal)
                     _                -> PDFString (encodePDFStringValue newVal)
          newDict = Map.insert "V" pdfVal d
      in Right ((ref, newDict) : objs, maxN)

-- ---------------------------------------------------------------------------
-- Incremental update writer

-- | Append new object versions and an updated xref/trailer to @pdfBytes@.
appendIncrementalUpdate
  :: Encryptor
  -> ByteString
  -> Int64                                           -- previous xref offset
  -> Map ByteString PDFValue                         -- original trailer
  -> [(ObjRef, Map ByteString PDFValue)]             -- updated objects
  -> ByteString
appendIncrementalUpdate enc pdfBytes prevXrefOff origTrailer updatedObjs =
  let baseLen  = fromIntegral (BS.length pdfBytes)
      -- Serialize each updated object and record its new offset.
      (objBlocks, offsets) = buildObjBlocks enc baseLen updatedObjs
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
  :: Encryptor
  -> Int64
  -> [(ObjRef, Map ByteString PDFValue)]
  -> ([BB.Builder], [(ObjRef, Int64)])
buildObjBlocks enc startOff objs =
  let go off [] = ([], [])
      go off ((ref@(n,g), dict) : rest) =
        let block   = serializeObj n g (encryptPDFValues enc n g dict)
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
  PDFReal r       -> BB.string7 (showFFloat Nothing r "")
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
              -- Carry over document-level entries from the original trailer.
              -- /Encrypt and /ID must both be present when the source PDF is
              -- encrypted: /Encrypt tells readers to decrypt the original body
              -- objects (pages, fonts, etc.), and /ID is required by the
              -- Standard Security Handler algorithm to derive the file key
              -- (PDF spec §7.6.3.3 Algorithm 2).  Omitting either causes
              -- readers to either prompt for a password or fail to read the
              -- original page tree.
              [ (k, v)
              | k <- ["Root", "Info", "Encrypt", "ID"]
              , Just v <- [Map.lookup k origTrailer]
              ]
  in LBS.toStrict $ BB.toLazyByteString $
       "trailer\n" <> serializeDict td <> "\n"

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
  -> Decryptor
  -> ByteString                    -- ^ key name
  -> Map ByteString PDFValue
  -> Either String [PDFValue]
loadArray bs xref dec key d =
  case Map.lookup key d of
    Just (PDFArray a) -> Right a
    Just ref@PDFRef{} -> do
      v <- loadObject bs xref dec ref
      case v of
        PDFArray a -> Right a
        _          -> Left ("/" <> BSC.unpack key <> " reference is not an array")
    Just _ -> Left ("/" <> BSC.unpack key <> " is not an array")
    Nothing -> Left ("/" <> BSC.unpack key <> " not found in dict")
