{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Parsing of PDF values using 'Text.Grampa.PEG.Backtrack'.
--
-- References: PDF 32000-1:2008 (PDF 1.7 specification):
--
-- * §7.3 – Objects (booleans, numbers, strings, names, arrays, dictionaries)

module Text.FDF.PDF.Parse (
  parseDict, parseIndirectObject, parseValue,
  hexDigit, dropLineEnd, dropWS, dropWS1, readDecimal
) where

import Prelude hiding (takeWhile)
import Control.Applicative ((<|>), many, optional)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import Data.Char (isDigit, isHexDigit, isSpace, ord)
import Data.Int (Int64)
import Data.List (intercalate)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Word (Word8)
import Rank2 qualified
import Text.Grampa (InputParsing (..), ParseFailure (..), FailureDescription (..))
import Text.Grampa.PEG.Backtrack qualified as PEG

import Text.FDF.PDF.Types

-- ---------------------------------------------------------------------------
-- Parser type

-- | Backtracking PEG parser for PDF value fragments, operating directly on
-- raw 'ByteString' input (byte-level parsing, no UTF-8 interpretation).
type PDFParser = PEG.Parser (Rank2.Only PDFValue) ByteString

-- | Run a 'PDFParser', returning the result and the remaining (unconsumed)
-- input on success, or an error message on failure.
runParser :: PDFParser a -> ByteString -> Either String (a, ByteString)
runParser p input = case PEG.applyParser p input of
  PEG.Parsed v rest -> Right (v, rest)
  PEG.NoParse (ParseFailure _ (FailureDescription descs lits) errs) ->
    Left $ case descs ++ map BSC.unpack lits ++ errs of
      []   -> "Parse failure"
      msgs -> "Expected: " ++ intercalate ", " msgs

-- ---------------------------------------------------------------------------
-- Exported parsing entry points

type ParseResult a = Either String (a, ByteString)

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

-- | Parse any PDF value, skipping leading whitespace.
parseValue :: ByteString -> ParseResult PDFValue
parseValue bs = runParser pdfValue bs

-- | Parse a PDF dictionary (@\<\< ... \>\>@), skipping leading whitespace.
parseDict :: ByteString -> ParseResult (Map ByteString PDFValue)
parseDict bs = runParser pdfDict bs

-- ---------------------------------------------------------------------------
-- Byte-level predicates (operating on single-byte 'ByteString' factors)

-- | A single-byte factor is a PDF whitespace character.
isPDFWSByte :: ByteString -> Bool
isPDFWSByte b = case BSC.uncons b of
  Just (c, _) -> isPDFWS c
  Nothing     -> False

-- | A single-byte factor is valid inside a PDF name token.
isNameByte' :: ByteString -> Bool
isNameByte' b = case BSC.uncons b of
  Just (c, _) -> not (isSpace c) && c `notElem` ("/()<>[]{}%\0" :: String)
  Nothing     -> False

-- | A single-byte factor is an ASCII decimal digit.
isDigitByte :: ByteString -> Bool
isDigitByte b = case BSC.uncons b of
  Just (c, _) -> isDigit c
  Nothing     -> False

-- | A single-byte factor is an octal digit (@0@–@7@).
isOctByte :: ByteString -> Bool
isOctByte b = case BSC.uncons b of
  Just (c, _) -> c >= '0' && c <= '7'
  Nothing     -> False

-- ---------------------------------------------------------------------------
-- Core PDF value parser

-- | Parser for any PDF value; skips leading whitespace.
pdfValue :: PDFParser PDFValue
pdfValue = skipWS *>
  (   PDFNull       <$  string "null"
  <|> PDFBool True  <$  string "true"
  <|> PDFBool False <$  string "false"
  -- PDF names may be empty (e.g. empty-selection state serialised as @\/@).
  <|> PDFName       <$> (string "/" *> takeWhile isNameByte')
  <|> PDFString     <$> pdfLiteralString
  -- Try dict (<<) before hex string (<).
  <|> PDFDict       <$> pdfDict
  <|> PDFString     <$> pdfHexString
  <|> PDFArray      <$> pdfArray
  <|> pdfNumOrRef
  )

-- | Skip zero or more PDF whitespace bytes.
skipWS :: PDFParser ByteString
skipWS = takeWhile isPDFWSByte

-- ---------------------------------------------------------------------------
-- Literal string

-- | Parse a PDF literal string, consuming the opening @(@ and closing @)@.
pdfLiteralString :: PDFParser ByteString
pdfLiteralString = string "(" *> pdfLiteralContent <* string ")"

-- | Parse the content of a literal string up to the matching closing @)@.
-- Handles nested parentheses and escape sequences.
pdfLiteralContent :: PDFParser ByteString
pdfLiteralContent = fmap mconcat $ many litChunk
  where
    litChunk =
          litEscape
      <|> litNested
      <|> takeWhile1 isRegularLitByte
    -- A nested pair @(...)@ is kept verbatim in the string value.
    litNested = do
      _ <- string "("
      inner <- pdfLiteralContent
      _ <- string ")"
      return ("(" <> inner <> ")")
    -- Any byte other than @(@, @)@, or @\@ is a regular literal byte.
    isRegularLitByte b = b /= "(" && b /= ")" && b /= "\\"

-- | Parse a backslash escape sequence inside a literal string.
litEscape :: PDFParser ByteString
litEscape = string "\\" *>
  (   "\n"  <$ string "n"
  <|> "\r"  <$ string "r"
  <|> "\t"  <$ string "t"
  <|> "\b"  <$ string "b"
  <|> "\f"  <$ string "f"
  <|> "("   <$ string "("
  <|> ")"   <$ string ")"
  <|> "\\"  <$ string "\\"
  -- Line continuation: \r\n or \r or \n → empty string
  <|> ""    <$ (string "\r" *> optional (string "\n"))
  <|> ""    <$ string "\n"
  <|> pdfOctalEscape
  -- Any other byte after backslash is kept as-is (PDF §7.3.4.2).
  <|> anyToken
  )

-- | Parse 1–3 octal digits and return the corresponding byte value.
pdfOctalEscape :: PDFParser ByteString
pdfOctalEscape = do
  d1 <- octDigitVal
  d2 <- optional octDigitVal
  d3 <- case d2 of
          Nothing -> return Nothing
          Just _  -> optional octDigitVal
  let n :: Word8 = fromIntegral $ case (d2, d3) of
        (Nothing, _)       -> d1
        (Just b2, Nothing) -> d1 * 8 + b2
        (Just b2, Just b3) -> (d1 * 8 + b2) * 8 + b3
  return (BS.singleton n)

-- | Parse a single octal digit, returning its numeric value.
octDigitVal :: PDFParser Int
octDigitVal = fmap (\b -> ord (BSC.head b) - 0x30) (satisfy isOctByte)

-- ---------------------------------------------------------------------------
-- Hex string

-- | Parse a PDF hex string, consuming the opening @<@ and closing @>@.
pdfHexString :: PDFParser ByteString
pdfHexString = string "<" *> pdfHexBody

-- | Parse the body and closing @>@ of a hex string.
pdfHexBody :: PDFParser ByteString
pdfHexBody = fmap BS.pack (many hexBytePair) <* skipWS <* string ">"
  where
    hexBytePair = do
      _ <- skipWS
      h1 <- hexNibble
      _ <- skipWS
      -- The second nibble is optional; a lone nibble is padded with 0
      -- (PDF §7.3.4.3).
      h2 <- hexNibble <|> pure 0
      return (fromIntegral (h1 * 16 + h2) :: Word8)
    hexNibble =
      fmap (\b -> hexDigit (BSC.head b))
        (satisfy (\b -> case BSC.uncons b of
                          Just (c, _) -> isHexDigit c
                          Nothing     -> False))

-- ---------------------------------------------------------------------------
-- Array

-- | Parse a PDF array, consuming the opening @[@ and closing @]@.
pdfArray :: PDFParser [PDFValue]
pdfArray = string "[" *> many pdfValue <* skipWS <* string "]"

-- ---------------------------------------------------------------------------
-- Dictionary

-- | Parse a PDF dictionary (@\<\< ... \>\>@).
pdfDict :: PDFParser (Map ByteString PDFValue)
pdfDict = string "<<" *> pdfDictBody

-- | Parse the body and closing @>>@ of a dictionary.
pdfDictBody :: PDFParser (Map ByteString PDFValue)
pdfDictBody = fmap Map.fromList (many pdfEntry) <* skipWS <* string ">>"
  where
    pdfEntry = do
      _ <- skipWS *> string "/"
      -- Dictionary keys must be non-empty names.
      name  <- takeWhile1 isNameByte'
      value <- pdfValue
      return (name, value)

-- ---------------------------------------------------------------------------
-- Numbers and indirect references

-- | Parse an unsigned decimal integer (one or more digits).
pdfUnsignedInt :: PDFParser Int
pdfUnsignedInt = fmap (maybe 0 fst . BSC.readInt) (takeWhile1 isDigitByte)

-- | Parse an indirect object reference (@N G R@), a real number, or an integer.
pdfNumOrRef :: PDFParser PDFValue
pdfNumOrRef = pdfRef <|> pdfNum

-- | Try to parse an indirect object reference (@N G R@).
-- Object and generation numbers must be unsigned (positive) integers.
pdfRef :: PDFParser PDFValue
pdfRef = do
  n <- pdfUnsignedInt
  _ <- skipWS
  m <- pdfUnsignedInt
  _ <- skipWS
  _ <- string "R"
  return (PDFRef n m)

-- | Parse a signed integer or real number (with optional scientific exponent).
pdfNum :: PDFParser PDFValue
pdfNum = do
  negative  <- (True <$ string "-") <|> (False <$ string "+") <|> pure False
  intDigits <- takeWhile1 isDigitByte
  let n       = maybe 0 fst (BSC.readInt intDigits)
      signedN = if negative then negate n else n
  (PDFReal <$> pdfFloatTail (fromIntegral signedN))
    <|> pure (PDFInt signedN)

-- | Parse the fractional part (@.digits@) and optional exponent of a real.
pdfFloatTail :: Double -> PDFParser Double
pdfFloatTail intPart = do
  _ <- string "."
  fracDigits <- takeWhile isDigitByte
  let fracN = maybe 0 fst (BSC.readInt fracDigits)
      dVal  = intPart + fromIntegral fracN / (10.0 ^ BS.length fracDigits)
  pdfExpTail dVal

-- | Consume an optional scientific-notation exponent (@e±N@ or @E±N@).
--
-- If the @e@\/@E@ is not followed by at least one digit (e.g. a bare @e@
-- that begins a subsequent keyword like @endobj@), the @e@\/@E@ is left
-- unconsumed and the value is returned unchanged.
pdfExpTail :: Double -> PDFParser Double
pdfExpTail dVal =
  ( do _ <- satisfy (\b -> b == "e" || b == "E")
       expSign <- ((-1) <$ string "-") <|> (1 <$ string "+") <|> pure 1
       expDigits <- takeWhile1 isDigitByte
       let e = expSign * maybe 0 fst (BSC.readInt expDigits)
       return (dVal * 10.0 ** fromIntegral (e :: Int))
  ) <|> pure dVal

-- ---------------------------------------------------------------------------
-- Whitespace / utility helpers

-- | Drop PDF whitespace (space, tab, CR, LF, FF, NUL) from the front.
dropWS :: ByteString -> ByteString
dropWS = BSC.dropWhile isPDFWS

-- | Drop exactly one PDF whitespace character, or nothing.
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

-- | Read a non-negative decimal integer from a 'ByteString', failing with
-- an error if the input does not start with at least one digit.
readDecimal :: ByteString -> Either String Int
readDecimal bs = case BSC.readInt bs of
  Just (n, _) -> Right n
  Nothing     -> Left ("Expected decimal integer, got: " <> BSC.unpack (BS.take 10 bs))

-- | Convert a hex digit character to its integer value (0–15).
hexDigit :: Char -> Int
hexDigit c
  | c >= '0' && c <= '9' = ord c - ord '0'
  | c >= 'a' && c <= 'f' = ord c - ord 'a' + 10
  | c >= 'A' && c <= 'F' = ord c - ord 'A' + 10
  | otherwise             = 0
