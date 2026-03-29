{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Parsing of PDF values
--
-- References: PDF 32000-1:2008 (PDF 1.7 specification):
--
-- * §7.3 – Objects (booleans, numbers, strings, names, arrays, dictionaries)

module Text.FDF.PDF.Parse (
  parseDict, parseIndirectObject, parseValue,
  hexDigit, dropLineEnd, dropWS, dropWS1, readDecimal
) where

import Control.Monad (when)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import Data.Char (chr, isDigit, isHexDigit, isSpace, ord)
import Data.Int (Int64)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Word (Word8)

import Text.FDF.PDF.Types


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
       -- PDF names may be empty (e.g. the empty-selection state serialised as @\/@).
       '/'  -> let (nm, rest) = BS.span isNameByte (BS.tail bs)
               in Right (PDFName nm, rest)
       '('  -> (\(s,  r) -> (PDFString s, r)) <$> parseLiteralString (BS.tail bs) 0
       '<'  ->
         if BS.length bs >= 2 && BSC.index bs 1 == '<'
           then (\(d, r) -> (PDFDict d, r)) <$> parseDict bs
           else (\(s, r) -> (PDFString s, r)) <$> parseHexString (BS.drop 1 bs)
       '['  -> (\(a, r) -> (PDFArray a, r)) <$> parseArray (BS.drop 1 bs)
       c | c == '-' || c == '+' || isDigit c -> parseNumOrRef bs
       _    -> Left ("Unexpected character: " <> [BSC.head bs])

-- | Parse a PDF name token (without the leading '/').
-- Returns an error for empty names, which are invalid as dictionary keys.
parseName :: ByteString -> ParseResult ByteString
parseName bs =
  let (nm, rest) = BS.span isNameByte bs
  in if BS.null nm
     then Left "Empty name"
     else Right (nm, rest)

-- | Predicate for bytes that are valid inside a PDF name token.
isNameByte :: Word8 -> Bool
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
parseNumOrRef bs0 = do
  let (sign, bs1) = case BSC.uncons bs0 of
                      Just ('-', r) -> ("-", r)
                      Just ('+', r) -> ("",  r)
                      _             -> ("",  bs0)
      (digits, rest) = BSC.span isDigit bs1
  when (BS.null digits) $ Left ("Expected number, got: " <> BSC.unpack (BS.take 10 bs0))
  n <- readDecimal digits
  let signedN = if sign == "-" then negate n else n
      rest' = dropWS rest
  case BSC.uncons rest' of
    Just ('.', afterDot) ->
      -- frac is the result of BSC.span isDigit, so it contains only digit
      -- characters; BSC.readInt succeeds unless frac is empty (e.g. "3."),
      -- in which case we treat the fractional part as zero.
      let (frac, rest'') = BSC.span isDigit afterDot
          fracN = maybe 0 fst (BSC.readInt frac)
          dVal  = fromIntegral signedN + fromIntegral fracN / (10 ^ BS.length frac)
      in parseOptionalExponent dVal rest''
    Just (c, _) | isDigit c && sign == "" -> do
      -- Could be "N G R" (indirect reference).
      let (gen, rest'') = BSC.span isDigit rest'
          rest''' = dropWS rest''
      genN <- readDecimal gen
      case BS.stripPrefix "R" rest''' of
        Just r  -> Right (PDFRef n genN, dropWS r)
        Nothing -> Right (PDFInt signedN, rest')
    _ -> Right (PDFInt signedN, rest')

-- | If the bytestring starts with an @e@/@E@ exponent, consume it and
-- return the adjusted 'PDFReal'; otherwise return the value as-is.
-- This handles scientific notation that may appear in PDFs from other tools
-- (e.g. @1.5e10@, @3.0E-2@) and also in our own round-trip when the
-- underlying @Double@ is serialised via @show@.
parseOptionalExponent :: Double -> ByteString -> ParseResult PDFValue
parseOptionalExponent dVal bs =
  case BSC.uncons bs of
    Just (c, afterE) | c == 'e' || c == 'E' ->
      let (expSign, afterSign) = case BSC.uncons afterE of
                                   Just ('+', r) -> (1,    r)
                                   Just ('-', r) -> (-1,   r)
                                   _             -> (1,    afterE)
          (expDigits, rest') = BSC.span isDigit afterSign
      in if BS.null expDigits
           then Right (PDFReal dVal, bs)  -- bare 'e' that is not an exponent; leave it
           else case BSC.readInt expDigits of
                  Just (e, _) -> Right (PDFReal (dVal * (10.0 ** fromIntegral (expSign * e))), rest')
                  Nothing     -> Right (PDFReal dVal, bs)
    _ -> Right (PDFReal dVal, bs)

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

-- | Read a non-negative decimal integer from a 'ByteString', failing with
-- an error if the input does not start with at least one digit.
readDecimal :: ByteString -> Either String Int
readDecimal bs = case BSC.readInt bs of
  Just (n, _) -> Right n
  Nothing     -> Left ("Expected decimal integer, got: " <> BSC.unpack (BS.take 10 bs))
