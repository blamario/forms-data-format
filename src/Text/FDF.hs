{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Text.FDF (FDF (FDF, body), Field (Field, name, value, kids),
                 mapWithKey, mapFieldWithKey,
                 foldMapWithKey, foldMapFieldWithKey,
                 traverseWithKey, traverseFieldWithKey,
                 parse, serialize) where

import Control.Applicative ((<*), (<*>), (<|>), many, optional)
import Data.ByteString (ByteString)
import Data.Char (isSpace)
import Data.Monoid.Instances.ByteString.UTF8 (ByteStringUTF8 (ByteStringUTF8))
import Data.Monoid.Textual (toText)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding (encodeUtf8)
import Text.ParserCombinators.Incremental.LeftBiasedLocal

data FDF = FDF {
  header :: ByteString,
  body :: Field,
  trailer :: ByteString}
  deriving (Show)

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
  <> "<<\n/FDF\n"
  <> encodeUtf8 (serializeField body) <> "\n"
  <> ">>\n"
  <> trailer
  <> "%%EOF\n"

serializeField :: Field -> Text
serializeField Field{name, value, kids} =
  "<<\n"
  <> "/T (" <> name <> ")\n"
  <> foldMap (\v-> "/T (" <> v <> ")\n") value
  <> if null kids then "" else "/Kids[\n" <> Text.intercalate "\n" (serializeField <$> kids) <> "]\n"
  <> ">>"

parse :: ByteString -> Either String FDF
parse = verify . inspect . feedEof . flip feed parser . ByteStringUTF8
  where verify (Right ([(parsed, "")], Nothing)) = Right parsed
        verify (Right _) = Left "Internal parse error"
        verify (Left err) = Left err

parser :: Parser ByteStringUTF8 FDF
parser = FDF
  <$ string "%FDF-1.2" <* lineEnd
  <*> extract (manyTill line begin)
  <* begin
  <* string "/FDF" <* lineEnd
  <*> field
  <* end
  <*> extract (string "endobj" <> lineEnd
               <> takeCharsWhile isSpace
               <> string "trailer" <> lineEnd
               <> manyTill line (string "%%EOF"))
  <* string "%%EOF"
  <* optional lineEnd

field :: Parser ByteStringUTF8 Field
field = Field <$ begin
  <*> strictText (string "/T (" *> takeCharsWhile (`notElem` [')', '\r', '\n']) <* string ")" <* lineEnd)
  <*> optional (strictText $ string "/V (" *> takeCharsWhile (`notElem` [')', '\r', '\n']) <* string ")" <* lineEnd)
  <*> moptional (string "/Kids [" *> lineEnd *> many field <* string "]" <* lineEnd) -- kids
  <* end

begin :: Parser ByteStringUTF8 ()
begin = skip (string "<<" *> lineEnd)

end :: Parser ByteStringUTF8 ()
end = skip (string ">>" *> optional lineEnd)

line :: Parser ByteStringUTF8 ByteStringUTF8
line = takeCharsWhile (`notElem` ['\r', '\n']) <> lineEnd

lineEnd :: Parser ByteStringUTF8 ByteStringUTF8
lineEnd = string "\r\n" <|> string "\r" <|> string "\n"

strictText :: Parser ByteStringUTF8 ByteStringUTF8 -> Parser ByteStringUTF8 Text
strictText = fmap $ toText (error . ("Invalid UTF-8 sequence: " ++) . show)

extract :: Parser ByteStringUTF8 ByteStringUTF8 -> Parser ByteStringUTF8 ByteString
extract = fmap $ \(ByteStringUTF8 bs) -> bs
