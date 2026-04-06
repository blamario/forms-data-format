{-# LANGUAGE Haskell2010, ImportQualifiedPost, OverloadedStrings #-}

-- | Convert a PDF file with AcroForm fields into an FDF file containing
-- the text labels drawn near each form field, rather than their values.

module Main (main) where

import Control.Applicative ((<|>), optional)
import Data.ByteString qualified as ByteString
import Data.Maybe (fromMaybe)
import Options.Applicative qualified as OptsAp

import Text.FDF (FDF (..), Field (..), FieldContent (..))
import Text.FDF qualified as FDF
import Text.FDF.PDF (parsePDF, fieldLabelsWith, LabelConfig (..), SearchZone (..), defaultLabelConfig)

data Options = Options
  { input     :: FilePath
  , output    :: FilePath
  , optLeft   :: Maybe Double
  , optRight  :: Maybe Double
  , optAbove  :: Maybe Double
  , optBelow  :: Maybe Double
  }

optionsParser :: OptsAp.Parser Options
optionsParser = Options
  <$> OptsAp.strArgument
        (OptsAp.metavar "<input.pdf>"
         <> OptsAp.help "Input PDF file (use - for stdin)")
  <*> (OptsAp.strArgument
         (OptsAp.metavar "<output.fdf>"
          <> OptsAp.help "Output FDF file (default: write to stdout)")
       <|> pure "-")
  <*> optional (OptsAp.option OptsAp.auto
        (OptsAp.long "left"
         <> OptsAp.metavar "POINTS"
         <> OptsAp.help "Search margin to the left of each field box (default: 60)"))
  <*> optional (OptsAp.option OptsAp.auto
        (OptsAp.long "right"
         <> OptsAp.metavar "POINTS"
         <> OptsAp.help "Search margin to the right of each field box (default: 60)"))
  <*> optional (OptsAp.option OptsAp.auto
        (OptsAp.long "above"
         <> OptsAp.metavar "POINTS"
         <> OptsAp.help "Search margin above each field box (default: 60)"))
  <*> optional (OptsAp.option OptsAp.auto
        (OptsAp.long "below"
         <> OptsAp.metavar "POINTS"
         <> OptsAp.help "Search margin below each field box (default: 60)"))

-- | Build a 'LabelConfig' from command-line options, falling back to
-- 'defaultLabelConfig' margins for any values not specified.
buildConfig :: Options -> LabelConfig
buildConfig opts =
  let defZone = lcSearchZone defaultLabelConfig
  in defaultLabelConfig
       { lcSearchZone = SearchZone
           { zoneLeft  = fromMaybe (zoneLeft defZone)  (optLeft opts)
           , zoneRight = fromMaybe (zoneRight defZone) (optRight opts)
           , zoneAbove = fromMaybe (zoneAbove defZone) (optAbove opts)
           , zoneBelow = fromMaybe (zoneBelow defZone) (optBelow opts)
           }
       }

main :: IO ()
main = do
  opts <- OptsAp.execParser $
    OptsAp.info (optionsParser OptsAp.<**> OptsAp.helper)
      (OptsAp.fullDesc
       <> OptsAp.progDesc "Extract text labels near AcroForm fields from a PDF into an FDF file"
       <> OptsAp.header "pdf-to-labels - extract form field labels from PDF")
  pdfBytes <- if input opts == "-"
                then ByteString.getContents
                else ByteString.readFile (input opts)
  pdf <- case parsePDF pdfBytes of
    Left err  -> ioError (userError $ "Error reading PDF: " <> err)
    Right pdf -> return pdf
  let config = buildConfig opts
  labels <- case fieldLabelsWith config pdf of
    Left err  -> ioError (userError $ "Error extracting labels: " <> err)
    Right ls  -> return ls
  let fdfBody = case labels of
        [f] -> f
        _   -> Field { name = "", content = Children labels }
      fdf = FDF
        "1 0 obj\n"
        fdfBody
        "endobj\ntrailer\n\n<<\n/Root 1 0 R\n>>\n"
      fdfBytes = FDF.serialize fdf
  if output opts == "-"
    then ByteString.putStr fdfBytes
    else ByteString.writeFile (output opts) fdfBytes
