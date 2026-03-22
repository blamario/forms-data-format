{-# LANGUAGE Haskell2010, ImportQualifiedPost, OverloadedStrings #-}

-- | Fill the AcroForm fields of a PDF using an FDF file.

module Main (main) where

import Data.ByteString qualified as ByteString
import Options.Applicative qualified as OptsAp

import Text.FDF qualified as FDF
import Text.FDF.PDF (fillPDF)

data Options = Options
  { fdfInput  :: FilePath
  , pdfInput  :: FilePath
  , pdfOutput :: FilePath
  }

optionsParser :: OptsAp.Parser Options
optionsParser = Options
  <$> OptsAp.strArgument
        (OptsAp.metavar "<input.fdf>"
         <> OptsAp.help "FDF file containing form field values")
  <*> OptsAp.strArgument
        (OptsAp.metavar "<template.pdf>"
         <> OptsAp.help "PDF template file whose AcroForm fields are to be filled")
  <*> (OptsAp.strArgument
         (OptsAp.metavar "<output.pdf>"
          <> OptsAp.help "Output PDF file (default: write to stdout)")
       OptsAp.<|> pure "-")

throwError :: String -> IO a
throwError = ioError . userError

main :: IO ()
main = do
  opts <- OptsAp.execParser $
    OptsAp.info (optionsParser OptsAp.<**> OptsAp.helper)
      (OptsAp.fullDesc
       <> OptsAp.progDesc "Fill AcroForm fields of a PDF template with values from an FDF file"
       <> OptsAp.header "fdf-to-pdf - fill PDF from FDF")
  fdfBytes <- ByteString.readFile (fdfInput opts)
  pdfBytes <- ByteString.readFile (pdfInput opts)
  fdf <- case FDF.parse fdfBytes of
    Left err  -> throwError $ "Error parsing FDF: " <> err
    Right fdf -> return fdf
  case fillPDF fdf pdfBytes of
    Left err     -> throwError $ "Error filling PDF: " <> err
    Right filled ->
      if pdfOutput opts == "-"
        then ByteString.putStr filled
        else ByteString.writeFile (pdfOutput opts) filled
