{-# LANGUAGE Haskell2010, ImportQualifiedPost, OverloadedStrings #-}

-- | Convert a PDF file with AcroForm fields into an FDF file.
--
-- Usage: pdf-to-fdf <input.pdf> [<output.fdf>]
--
-- If the output path is omitted or @-@, the FDF is written to standard
-- output.

module Main (main) where

import Control.Monad (unless)
import Data.ByteString qualified as ByteString
import System.Directory (doesFileExist)
import System.Environment (getArgs)
import System.Exit (exitFailure)
import System.IO (hPutStrLn, stderr)

import Text.FDF qualified as FDF
import Text.FDF.PDF (parsePDF)

main :: IO ()
main = do
  args <- getArgs
  case args of
    [input]         -> run input "-"
    [input, output] -> run input output
    _               -> do
      hPutStrLn stderr "Usage: pdf-to-fdf <input.pdf> [<output.fdf>]"
      exitFailure

run :: FilePath -> FilePath -> IO ()
run inputPath outputPath = do
  unless (inputPath == "-") $ do
    exists <- doesFileExist inputPath
    unless exists $ do
      hPutStrLn stderr $ "File not found: " <> inputPath
      exitFailure
  pdfBytes <- if inputPath == "-"
                then ByteString.getContents
                else ByteString.readFile inputPath
  case parsePDF pdfBytes of
    Left err  -> do
      hPutStrLn stderr $ "Error reading PDF: " <> err
      exitFailure
    Right fdf -> do
      let fdfBytes = FDF.serialize fdf
      if outputPath == "-"
        then ByteString.putStr fdfBytes
        else ByteString.writeFile outputPath fdfBytes
