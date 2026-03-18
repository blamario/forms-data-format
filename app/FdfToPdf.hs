{-# LANGUAGE Haskell2010, ImportQualifiedPost, OverloadedStrings #-}

-- | Fill the AcroForm fields of a PDF using an FDF file.
--
-- Usage: fdf-to-pdf <input.fdf> <template.pdf> [<output.pdf>]
--
-- If the output path is omitted or @-@, the filled PDF is written to
-- standard output.

module Main (main) where

import Control.Monad (unless)
import Data.ByteString qualified as ByteString
import System.Directory (doesFileExist)
import System.Environment (getArgs)
import System.Exit (exitFailure)
import System.IO (hPutStrLn, stderr)

import Text.FDF qualified as FDF
import Text.FDF.PDF (fillPDF)

main :: IO ()
main = do
  args <- getArgs
  case args of
    [fdfIn, pdfIn]         -> run fdfIn pdfIn "-"
    [fdfIn, pdfIn, pdfOut] -> run fdfIn pdfIn pdfOut
    _                      -> do
      hPutStrLn stderr "Usage: fdf-to-pdf <input.fdf> <template.pdf> [<output.pdf>]"
      exitFailure

run :: FilePath -> FilePath -> FilePath -> IO ()
run fdfPath pdfPath outputPath = do
  mapM_ checkExists [fdfPath, pdfPath]
  fdfBytes <- ByteString.readFile fdfPath
  pdfBytes <- ByteString.readFile pdfPath
  fdf <- case FDF.parse fdfBytes of
    Left err  -> do
      hPutStrLn stderr $ "Error parsing FDF: " <> err
      exitFailure
    Right fdf -> return fdf
  case fillPDF fdf pdfBytes of
    Left err   -> do
      hPutStrLn stderr $ "Error filling PDF: " <> err
      exitFailure
    Right filled ->
      if outputPath == "-"
        then ByteString.putStr filled
        else ByteString.writeFile outputPath filled

checkExists :: FilePath -> IO ()
checkExists path = do
  exists <- doesFileExist path
  unless exists $ do
    hPutStrLn stderr $ "File not found: " <> path
    exitFailure
