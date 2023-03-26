{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Text.FDF (FDF (FDF, body), Field (Field, name, value, kids),
                 mapWithKey, mapFieldWithKey,
                 foldMapWithKey, foldMapFieldWithKey,
                 traverseWithKey, traverseFieldWithKey,
                 parse, serialize) where

import Control.Applicative ((<*), (<*>), (<|>), many, optional)
import Data.Char (isSpace)
import Data.Text (Text)
import Data.Text qualified as Text
import Text.ParserCombinators.Incremental.LeftBiasedLocal

data FDF = FDF {
  header :: Text,
  body :: Field,
  trailer :: Text}
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

serialize :: FDF -> Text
serialize FDF{header, body, trailer} =
  "%FDF-1.2\n"
  <> header
  <> "<<\n/FDF\n"
  <> serializeField body <> "\n"
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

parse :: Text -> Either String FDF
parse = verify . inspect . feedEof . flip feed parser
  where verify (Right ([(parsed, "")], Nothing)) = Right parsed
        verify (Right _) = Left "Internal parse error"
        verify (Left err) = Left err

parser :: Parser Text FDF
parser = FDF
  <$ string "%FDF-1.2" <* lineEnd
  <*> (manyTill line begin)
  <* begin
  <* string "/FDF" <* lineEnd
  <*> field
  <* end
  <*> (string "endobj" <> lineEnd
       <> takeCharsWhile isSpace
       <> string "trailer" <> lineEnd
       <> manyTill line (string "%%EOF"))
  <* string "%%EOF"
  <* optional lineEnd

field :: Parser Text Field
field = Field <$ begin
  <*> (string "/T (" *> takeCharsWhile (`notElem` [')', '\r', '\n']) <* string ")" <* lineEnd) -- name
  <*> optional (string "/V (" *> takeCharsWhile (`notElem` [')', '\r', '\n']) <* string ")" <* lineEnd) -- value
  <*> moptional (string "/Kids [" *> lineEnd *> many field <* string "]" <* lineEnd) -- kids
  <* end

begin :: Parser Text ()
begin = skip (string "<<" *> lineEnd)

end :: Parser Text ()
end = skip (string ">>" *> optional lineEnd)

line :: Parser Text Text
line = takeCharsWhile (`notElem` ['\r', '\n']) <> lineEnd

lineEnd :: Parser Text Text
lineEnd = string "\r\n" <|> string "\r" <|> string "\n"
