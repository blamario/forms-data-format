{-# LANGUAGE Haskell2010, OverloadedRecordDot, OverloadedStrings, NoFieldSelectors  #-}

-- | Outputs the difference of two input FDF files in the following format:
--
-- output = (line "\n")*
-- line = "< " path ("=" value)?
--      | "> " path ("=" value)?
--      | "! " path ": " value "->" value
-- path = name ("/" name)*
-- name = <any printable character except "/" and "=">*
-- value = <any printable character except ">">*

module Main (main) where

import Control.Applicative ((<|>), some)
import Control.Monad (unless, when)
import Data.Bifunctor (first)
import qualified Data.ByteString as ByteString
import Data.Foldable (fold, traverse_)
import qualified Data.List as List
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text.IO
import qualified Options.Applicative as OptsAp
import System.Directory (doesFileExist)
import Text.FDF (FDF)
import qualified Text.FDF as FDF

data Difference
  = Deletion (Maybe Text)
  | Addition (Maybe Text)
  | Change (Maybe Text) (Maybe Text)
  deriving (Eq, Read, Show)

data Options = Options {
  ignore :: [Text],
  oldPaths :: Bool,
  old :: FilePath,
  new :: FilePath,
  verbose :: Bool}

optionsParser :: OptsAp.Parser Options
optionsParser =
  (
    Options
    <$> some (OptsAp.strOption (OptsAp.short 'i' <> OptsAp.long "ignore" <> OptsAp.metavar "<name to ignore>"
                                <> OptsAp.help "ignore the named difference in the field path"))
    <*> (OptsAp.flag' True (OptsAp.long "old" <> OptsAp.help "emit field paths from the old FDF")
         <|> OptsAp.flag' False (OptsAp.long "new" <> OptsAp.help "emit field paths from the new FDF"))
    <|>
    pure (Options [] True)
  )
  <*> OptsAp.strArgument (OptsAp.metavar "<old form file input>")
  <*> OptsAp.strArgument (OptsAp.metavar "<new form file input>")
  <*> OptsAp.switch (OptsAp.short 'v' <> OptsAp.long "verbose" <> OptsAp.help "also diff fields with empty values")

readFDF :: FilePath -> IO FDF
readFDF inputPath = do
   exists <- doesFileExist inputPath
   unless (inputPath == "-" || exists) (error $ "Input file " <> show inputPath <> " doesn't exist.")
   content <- if inputPath == "-" then ByteString.getContents else ByteString.readFile inputPath
   case FDF.parse content of
     Left err -> error ((if inputPath == "-" then "Standard input" else "File " <> inputPath)
                        <> " is not valid FDF:\n" <> err)
     Right fdf -> pure fdf

diffLine :: ([Text], Difference) -> Text
diffLine (path, Deletion value) = "< " <> Text.intercalate "/" path <> foldMap ("=" <>) value
diffLine (path, Addition value) = "> " <> Text.intercalate "/" path <> foldMap ("=" <>) value
diffLine (path, Change old new) = "! " <> Text.intercalate "/" path <> ": " <> fold old <> "->" <> fold new

diff :: Bool -> (Text -> Bool) -> [Text] -> FDF.Field -> FDF.Field -> [([Text], Difference)]
diff oldPaths ignorable ancestry old new
  | old.name /= new.name =
    map (Deletion <$>) (list (ancestry ++) old) ++
    map (Addition <$>) (list (ancestry ++) new)
  | old.value /= new.value =
    [(ancestry <> [old.name], Change old.value new.value)] <> diffAll oldPaths ignorable ancestry old.kids new.kids
  | otherwise = diffAll oldPaths ignorable (ancestry <> [old.name]) old.kids new.kids

diffAll, diffSorted :: Bool -> (Text -> Bool) -> [Text] -> [FDF.Field] -> [FDF.Field] -> [([Text], Difference)]

diffAll oldPaths ignorable ancestry old new = diffSorted oldPaths ignorable ancestry (List.sort old) (List.sort new)

diffSorted oldPaths ignorable ancestry (old : olds) (new : news)
  | ignorable old.name, Nothing <- old.value
  = diffSorted oldPaths ignorable
      (if oldPaths then ancestry ++ [old.name] else ancestry)
      (List.sort $ old.kids ++ olds)
      (new : news)
  | ignorable new.name, Nothing <- new.value
  = diffSorted oldPaths ignorable
      (if oldPaths then ancestry else ancestry ++ [new.name])
      (old : olds)
      (List.sort $ new.kids ++ news)
  | old.name < new.name = map (Deletion <$>) (list (ancestry ++) old) ++ diffSorted oldPaths ignorable ancestry olds (new : news)
  | old.name > new.name = map (Addition <$>) (list (ancestry ++) new) ++ diffSorted oldPaths ignorable ancestry (old : olds) news
  | otherwise = diff oldPaths ignorable ancestry old new <> diffSorted oldPaths ignorable ancestry olds news
diffSorted _ _ ancestry olds [] = foldMap (map (Deletion <$>) . list (ancestry ++)) olds
diffSorted _ _ ancestry [] news = foldMap (map (Addition <$>) . list (ancestry ++)) news

list :: ([Text] -> [Text]) -> FDF.Field -> [([Text], Maybe Text)]
list addAncestry x = (addAncestry [x.name], x.value) : foldMap (list (addAncestry . (x.name :))) (List.sort x.kids)

hasNonemptyValue :: Difference -> Bool
hasNonemptyValue (Deletion (Just v)) = not (Text.null v)
hasNonemptyValue (Addition (Just v)) = not (Text.null v)
hasNonemptyValue (Change v1 v2) = v1 /= v2
hasNonemptyValue _ = False

process :: Options -> IO ()
process options = do
  when (options.old == "-" && options.new == "-") $ error "Only one input can be '-' stdin"
  old <- readFDF options.old
  new <- readFDF options.new
  let filterEmpty = if options.verbose then id else filter (hasNonemptyValue . snd)
      ignorable name = maybe (name `elem` options.ignore) (`elem` options.ignore) (Text.stripSuffix "[0]" name)
  traverse_ (Text.IO.putStrLn . diffLine) (filterEmpty $ diff options.oldPaths ignorable [] old.body new.body)

main :: IO ()
main =
  OptsAp.execParser (OptsAp.info optionsParser
                     $ OptsAp.progDesc "Output the difference between two input FDF files")
  >>= process
