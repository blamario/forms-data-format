{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

-- | Parse and serialize between FDF files and `Map [Text] Text`.

module Text.FDF (FDF (FDF, body), Field (Field, name, value, kids),
                 mapWithKey, mapFieldWithKey,
                 foldMapWithKey, foldMapFieldWithKey,
                 traverseWithKey, traverseFieldWithKey,
                 parse, serialize) where

import Control.Applicative ((<*), (<*>), (<|>), many, some, optional)
import Data.Bifunctor (bimap)
import Data.ByteString (ByteString)
import Data.ByteString qualified as ByteString
import Data.Char (chr, digitToInt, isAscii, isSpace, ord)
import Data.Monoid.Instances.ByteString.UTF8 (ByteStringUTF8 (ByteStringUTF8))
import Data.Monoid.Textual (singleton, toString, toText)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding (decodeUtf16BE, encodeUtf8, encodeUtf16BE)
import Numeric (showOct)
import Rank2 qualified
import Text.Grampa
import Text.Grampa.Combinators
import Text.Parser.Char (octDigit)
import Text.Parser.Combinators (manyTill)
import Text.Grampa.PEG.Backtrack qualified as PEG

type Parser = PEG.Parser (Rank2.Only FDF)

-- | Parsed FDF data structure
data FDF = FDF {
  header :: ByteString,
  body :: Field,
  trailer :: ByteString}
  deriving (Show)

-- | The body of FDF is a tree of nestable 'Field's.
data Field = Field {
  name :: Text,
  value :: Maybe Text,
  kids :: [Field]}
  deriving (Show)

mapWithKey :: ([Text] -> Text -> Text) -> FDF -> FDF
mapWithKey f x@FDF{body} = x{body = mapFieldWithKey f body}

mapFieldWithKey :: ([Text] -> Text -> Text) -> Field -> Field
mapFieldWithKey f x@Field{name, value, kids} =
  x{value = f [name] <$> value,
    kids = mapFieldWithKey (f . (name:)) <$> kids}

foldMapWithKey :: Monoid a => ([Text] -> Text -> a) -> FDF -> a
foldMapWithKey f x@FDF{body} = foldMapFieldWithKey f body

foldMapFieldWithKey :: Monoid a => ([Text] -> Text -> a) -> Field -> a
foldMapFieldWithKey f x@Field{name, value, kids} =
  foldMap (f [name]) value <> foldMap (foldMapFieldWithKey $ f . (name:)) kids

traverseWithKey :: Applicative f => ([Text] -> Text -> f Text) -> FDF -> f FDF
traverseWithKey f x@FDF{body} = (\body'-> x{body = body'}) <$> traverseFieldWithKey f body

traverseFieldWithKey :: Applicative f => ([Text] -> Text -> f Text) -> Field -> f Field
traverseFieldWithKey f x@Field{name, value, kids} =
  Field name <$> traverse (f [name]) value <*> traverse (traverseFieldWithKey $ f . (name:)) kids

serialize :: FDF -> ByteString
serialize FDF{header, body, trailer} =
  "%FDF-1.2\n"
  <> header
  <> "<<\n"
  <> "/FDF\n"
  <> "<<\n"
  <> "/Fields [\n"
  <> serializeField body <> "\n"
  <> "]\n"
  <> ">>\n"
  <> ">>\n"
  <> trailer
  <> "%%EOF\n"

serializeField :: Field -> ByteString
serializeField Field{name, value, kids} =
  "<<\n"
  <> "/T (" <> encodeUtf8 name <> ")\n"
  <> foldMap (\v-> "/V (" <> serializeValue v <> ")\n") value
  <> (if null kids then "" else "/Kids [\n" <> ByteString.intercalate "\n" (serializeField <$> kids) <> "]\n")
  <> ">>"

serializeValue :: Text -> ByteString
serializeValue t
  | Text.isAscii t = encodeUtf8 (plain <> escaped)
  | otherwise = utf16beBOM <> encodeUtf16BE t
  where (plain, special) = Text.span (\c -> c >= ' ' && c `notElem` ['(', ')', '\\']) t
        escaped = Text.concatMap escape special
        escape '(' = Text.pack "\\("
        escape ')' = "\\)"
        escape '\\' = "\\\\"
        escape '\n' = "\\n"
        escape '\r' = "\\r"
        escape '\t' = "\\t"
        escape '\b' = "\\b"
        escape c
          | c < ' ' = "\\" <> Text.justifyRight 3 '0' (Text.pack $ showOct (ord c) "")
          | otherwise = Text.singleton c

parse :: ByteString -> Either String FDF
parse input =
  bimap (\failure-> toString (const "<?>") $ failureDescription s failure 4) id $ simply parseComplete parser s
  where s = ByteStringUTF8 input

parser :: Parser ByteStringUTF8 FDF
parser = FDF
  <$ (string "%FDF-1.2" <* lineEnd <?> "first line")
  <*> extract ((takeWhile1 (`notElem` ["\r", "\n"]) <?> "bytes")
               <> lineEnd <> (mconcat <$> manyTill line begin) <?> "header")
  <* (string "/FDF" <* takeCharsWhile (== ' ') <* lineEnd <?> "end header")
  <* begin
  <* (string "/Fields [" <* takeCharsWhile (== ' ') <* lineEnd <?> "fields")
  <*> field
  <* (string "]" <* takeCharsWhile (== ' ') <* lineEnd <?> "end the fields")
  <* (end <?> "end the body")
  <*> extract ((end <?> "end the object")
               <> string "endobj" <> lineEnd
               <> takeCharsWhile isSpace
               <> string "trailer" <> lineEnd
               <> (mconcat <$> manyTill line (string "%%EOF" <?> "last line"))
               <?> "trailer")
  <* optional lineEnd

field :: Parser ByteStringUTF8 Field
field = Field <$ begin
  <*> strictText (string "/T (" *> takeCharsWhile (`notElem` [')', '\r', '\n']) <* string ")" <* lineEnd <?> "name")
  <*> optional (strictText $
                admit (string "/V ("
                       *> commit ((string (ByteStringUTF8 utf16beBOM) *> (utf8from16 <$> Text.Grampa.takeWhile (/= ")"))
                                   <|> concatMany (takeCharsWhile1 (`notElem` [')', '\r', '\n', '\\']) <|> escape))
                                  <* string ")" <* lineEnd)
                       <|> string "/V /" *> commit (takeCharsWhile (`notElem` ['\r', '\n']) <* lineEnd)
                       <?> "value"))
  <*> admit (string "/Kids [" *> commit (lineEnd *> takeSome field <* string "]" <* lineEnd <?> "kids")
             <|> commit mempty)
  <* end
  where escape = char '\\'
                 *> (singleton <$> (char 'n' *> pure '\n'
                                    <|> char 'r' *> pure '\r'
                                    <|> char 't' *> pure '\t'
                                    <|> char 'b' *> pure '\b'
                                    <|> char 'f' *> pure '\f'
                                    <|> char '(' *> pure '('
                                    <|> char ')' *> pure ')'
                                    <|> char '\\' *> pure '\\'
                                    <|> chr . sum <$> sequenceA [(64 *) <$> octalDigit, (8 *) <$> octalDigit, octalDigit]))
        octalDigit = digitToInt <$> octDigit
        utf8from16 (ByteStringUTF8 bs) = ByteStringUTF8 (encodeUtf8 $ decodeUtf16BE bs)

begin :: Parser ByteStringUTF8 ByteStringUTF8
begin = string "<<" *> lineEnd <?> "<<"

end :: Parser ByteStringUTF8 ByteStringUTF8
end = string ">>" *> takeCharsWhile (== ' ') *> moptional lineEnd *> pure mempty <?> ">>"

line :: Parser ByteStringUTF8 ByteStringUTF8
line = takeCharsWhile (`notElem` ['\r', '\n']) <> lineEnd <?> "line"

lineEnd :: Parser ByteStringUTF8 ByteStringUTF8
lineEnd = string "\r\n" <|> string "\r" <|> string "\n"

strictText :: Parser ByteStringUTF8 ByteStringUTF8 -> Parser ByteStringUTF8 Text
strictText = fmap $ toText (error . ("Invalid UTF-8 sequence: " ++) . show)

extract :: Parser ByteStringUTF8 ByteStringUTF8 -> Parser ByteStringUTF8 ByteString
extract = fmap $ \(ByteStringUTF8 bs) -> bs

utf16beBOM :: ByteString
utf16beBOM = "\xFE\xFF"
