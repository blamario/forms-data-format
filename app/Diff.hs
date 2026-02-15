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
  old :: FilePath,
  new :: FilePath,
  verbose :: Bool}

optionsParser :: OptsAp.Parser Options
optionsParser =
   Options
   <$> OptsAp.strArgument (OptsAp.metavar "<old form file input>")
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

diff :: [Text] -> FDF.Field -> FDF.Field -> [([Text], Difference)]
diff ancestry old new
  | old.name /= new.name =
    map (Deletion <$>) (list (ancestry ++) old) ++
    map (Addition <$>) (list (ancestry ++) new)
  | old.value /= new.value =
    [(ancestry <> [old.name], Change old.value new.value)] <> diffAll ancestry old.kids new.kids
  | otherwise = diffAll (ancestry <> [old.name]) old.kids new.kids

diffAll, diffSorted :: [Text] -> [FDF.Field] ->  [FDF.Field] -> [([Text], Difference)]

diffAll ancestry old new = diffSorted ancestry (List.sort old) (List.sort new)

diffSorted ancestry (old : olds) (new : news)
  | old.name < new.name = map (Deletion <$>) (list (ancestry ++) old) ++ diffSorted ancestry olds (new : news)
  | old.name > new.name = map (Addition <$>) (list (ancestry ++) new) ++ diffSorted ancestry (old : olds) news
  | otherwise = diff ancestry old new <> diffSorted ancestry olds news
diffSorted ancestry olds [] = foldMap (map (Deletion <$>) . list (ancestry ++)) olds
diffSorted ancestry [] news = foldMap (map (Addition <$>) . list (ancestry ++)) news

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
  traverse_ (Text.IO.putStrLn . diffLine) (filterEmpty $ diff [] old.body new.body)

main :: IO ()
main =
  OptsAp.execParser (OptsAp.info optionsParser
                     $ OptsAp.progDesc "Output the difference between two input FDF files")
  >>= process
