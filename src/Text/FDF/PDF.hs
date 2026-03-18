{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

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

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Builder as BB
import qualified Data.ByteString.Char8 as BSC
import qualified Data.ByteString.Lazy as LBS
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

import Text.FDF (FDF (..), Field (..), FieldContent (..))
import qualified Text.FDF as FDF

-- ---------------------------------------------------------------------------
-- Public API

-- | Extract form field data from a PDF file.
--
-- Reads the PDF's AcroForm structure and returns the corresponding 'FDF'
-- value.  Supports PDFs with traditional (table-based) cross-reference
-- sections; cross-reference streams (PDF 1.5+) are not yet supported.
parsePDF :: ByteString -> Either String FDF
parsePDF bs = do
  xrefOff   <- findXRefOffset bs
  (xref, _) <- parseXRefChain bs xrefOff
  trailer   <- parseTrailerDict bs xrefOff
  rootRef   <- dictLookupRef "Root" trailer
  catalog   <- loadDict bs xref rootRef
  acroRef   <- dictLookupRef "AcroForm" catalog
  acroForm  <- loadDict bs xref acroRef
  fieldsArr <- loadArray bs xref "Fields" acroForm
  fields    <- mapM (loadFieldObj bs xref) fieldsArr
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
  xrefOff          <- findXRefOffset pdfBytes
  (xref, _)        <- parseXRefChain pdfBytes xrefOff
  trailer          <- parseTrailerDict pdfBytes xrefOff
  rootRef          <- dictLookupRef "Root" trailer
  catalog          <- loadDict pdfBytes xref rootRef
  acroRef          <- dictLookupRef "AcroForm" catalog
  acroForm         <- loadDict pdfBytes xref acroRef
  fieldsArr        <- loadArray pdfBytes xref "Fields" acroForm
  -- Build mapping:  full path → (objNum, current dict)
  pathMap          <- buildPathMap pdfBytes xref [] fieldsArr
  -- Collect leaf-value updates from FDF
  let updates       = collectUpdates [] (body fdf)
  -- Apply updates: produce list of (objNum, new dict)
  let totalObjs     = fromMaybe 0 $ do
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

-- | Byte offsets for in-use objects, keyed by object number.
type XRef = Map Int Int64

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

-- | Parse the full chain of cross-reference tables, following @/Prev@ links.
parseXRefChain :: ByteString -> Int64 -> Either String (XRef, Map ByteString PDFValue)
parseXRefChain bs off = do
  (xref, trailer) <- parseOneXRef bs off
  case Map.lookup "Prev" trailer of
    Just (PDFInt prev) -> do
      (prevXRef, _) <- parseXRefChain bs (fromIntegral prev)
      -- Newer (current) entries take precedence over older ones.
      return (Map.union xref prevXRef, trailer)
    _ -> return (xref, trailer)

-- | Parse a single cross-reference table and its trailer dictionary.
parseOneXRef :: ByteString -> Int64 -> Either String (XRef, Map ByteString PDFValue)
parseOneXRef bs off = do
  let chunk = BS.drop (fromIntegral off) bs
  if "xref" `BS.isPrefixOf` chunk
    then parseTraditionalXRef chunk
    else Left "Cross-reference streams (PDF 1.5+) are not yet supported"

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
parseSubsections bs xref
  | "trailer" `BS.isPrefixOf` bs = Right (bs, xref)
  | otherwise = do
      (bs', entries) <- parseSubsection bs
      parseSubsections bs' (Map.union entries xref)

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
                  [ (firstObj + i, parseXRefEntry (BS.take 20 (BS.drop (i * 20) r3)))
                  | i <- [0 .. count - 1]
                  ]
  Right (BS.drop (count * 20) r3, Map.filter (>= 0) entries)

-- | Parse one 20-byte xref entry.  Returns the byte offset, or -1 for free
-- entries.
parseXRefEntry :: ByteString -> Int64
parseXRefEntry entry
  | BS.length entry >= 18 && BSC.index entry 17 == 'f' = -1
  | otherwise = fromIntegral (readDecimal (BS.take 10 entry))

-- ---------------------------------------------------------------------------
-- Trailer dictionary helpers

-- | Parse the trailer dictionary at the given xref offset.
parseTrailerDict :: ByteString -> Int64 -> Either String (Map ByteString PDFValue)
parseTrailerDict bs xrefOff = do
  let chunk = BS.drop (fromIntegral xrefOff) bs
  -- Find "trailer" keyword inside the chunk
  case findFirst "trailer" chunk of
    Nothing -> Left "Cannot find 'trailer' keyword"
    Just off ->
      let afterTrailer = dropWS (BS.drop (off + 7) chunk)
      in case parseDict afterTrailer of
           Left err      -> Left ("Trailer dict parse error: " <> err)
           Right (td, _) -> Right td

findFirst :: ByteString -> ByteString -> Maybe Int
findFirst needle haystack
  | BS.null haystack          = Nothing
  | needle `BS.isPrefixOf` haystack = Just 0
  | otherwise                 = (+ 1) <$> findFirst needle (BS.tail haystack)

-- ---------------------------------------------------------------------------
-- Object loading

-- | Load and dereference an object.  References are followed one level deep.
loadObject :: ByteString -> XRef -> PDFValue -> Either String PDFValue
loadObject bs xref (PDFRef n _) = do
  off <- maybe (Left $ "Object " <> show n <> " not in xref") Right
               (Map.lookup n xref)
  parseIndirectObject bs off
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

dictLookupArray :: ByteString -> Map ByteString PDFValue -> Either String [PDFValue]
dictLookupArray key d =
  case Map.lookup key d of
    Just (PDFArray a) -> Right a
    Just _            -> Left ("/" <> BSC.unpack key <> " is not an array")
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
