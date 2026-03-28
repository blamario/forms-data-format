{-# LANGUAGE OverloadedStrings #-}
-- | Integration tests for the PDF round-trip: parsePDF ∘ fillPDF ≈ id.
module Main (main) where

import Control.Monad (unless)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import Data.IORef (IORef, newIORef, modifyIORef, readIORef)
import Data.List (foldl')
import System.Exit (exitFailure, exitSuccess)

import Text.FDF (FDF (..), Field (..), FieldContent (..))
import Text.FDF.PDF (parsePDF, fillPDF)

-- ---------------------------------------------------------------------------
-- Minimal test PDF construction

-- | Build a minimal valid PDF-1.4 byte string from a list of object bodies.
-- Objects are numbered starting from 1 with generation 0.
makePDF :: [BS.ByteString] -> BS.ByteString
makePDF contents =
  let header  = "%PDF-1.4\n"
      objects = zip [1..] contents
      (body, offsets) = foldl' addObj (header, []) objects
      xrefOff = BS.length body
      nObjs   = length objects
      xref    = BSC.pack $
                  "xref\n0 " <> show (nObjs + 1) <> "\n"
                  <> "0000000000 65535 f\r\n"
                  <> concatMap (\off -> padDec10 off <> " 00000 n\r\n") (reverse offsets)
      trailer = BSC.pack $
                  "trailer\n<< /Size " <> show (nObjs + 1)
                  <> " /Root 1 0 R >>\nstartxref\n"
                  <> show xrefOff <> "\n%%EOF\n"
  in body <> xref <> trailer
  where
    addObj (acc, offs) (n, content) =
      let objBS = BSC.pack (show (n :: Int) <> " 0 obj\n")
                  <> content <> "\nendobj\n"
      in (acc <> objBS, BS.length acc : offs)
    padDec10 n =
      let s = show n in replicate (10 - length s) '0' <> s

-- | A minimal PDF with a single text field @TextField1@ with value @Hello@.
simplePDF :: BS.ByteString
simplePDF = makePDF
  [ "<< /Type /Catalog /Pages 2 0 R /AcroForm 5 0 R >>"
  , "<< /Type /Pages /Kids [ 3 0 R ] /Count 1 >>"
  , "<< /Type /Page /Parent 2 0 R /MediaBox [0 0 612 792] /Annots [ 6 0 R ] >>"
  , "<< /Type /Font /Subtype /Type1 /BaseFont /Helvetica >>"
  , "<< /Fields [ 6 0 R ] /DR << /Font << /Helv 4 0 R >> >> >>"
  , "<< /Type /Annot /Subtype /Widget /FT /Tx /T (TextField1) /V (Hello) /Rect [100 700 400 720] /P 3 0 R /DA (/Helv 12 Tf 0 g) >>"
  ]

-- | Like 'simplePDF' but the field @\/Rect@ uses fractional coordinates
-- whose @show@ representation would involve scientific notation
-- (e.g. @0.05@ → @\"5.0e-2\"@).  This exercises the fix for the
-- \"Unexpected character: e\" bug in @serializeValue@ / @parseNumOrRef@.
floatRectPDF :: BS.ByteString
floatRectPDF = makePDF
  [ "<< /Type /Catalog /Pages 2 0 R /AcroForm 5 0 R >>"
  , "<< /Type /Pages /Kids [ 3 0 R ] /Count 1 >>"
  , "<< /Type /Page /Parent 2 0 R /MediaBox [0 0 612 792] /Annots [ 6 0 R ] >>"
  , "<< /Type /Font /Subtype /Type1 /BaseFont /Helvetica >>"
  , "<< /Fields [ 6 0 R ] /DR << /Font << /Helv 4 0 R >> >> >>"
  -- /Rect uses 0.05 – a value whose show representation is "5.0e-2"
  , "<< /Type /Annot /Subtype /Widget /FT /Tx /T (FloatField) /V (Hi) /Rect [0.05 0.05 200.05 12.05] /P 3 0 R /DA (/Helv 12 Tf 0 g) >>"
  ]

-- | A minimal PDF with a radio button group (initially unselected: @\/V \/@)
-- and a text field.  The radio group's child widget annotations have no @\/T@
-- key, which is the pattern that caused the original bug.
radioPDF :: BS.ByteString
radioPDF = makePDF
  [ "<< /Type /Catalog /Pages 2 0 R /AcroForm 5 0 R >>"
  , "<< /Type /Pages /Kids [ 3 0 R ] /Count 1 >>"
  , "<< /Type /Page /Parent 2 0 R /MediaBox [0 0 612 792] /Annots [ 7 0 R 8 0 R 9 0 R ] >>"
  , "<< /Type /Font /Subtype /Type1 /BaseFont /Helvetica >>"
  , "<< /Fields [ 6 0 R 9 0 R ] /DR << /Font << /Helv 4 0 R >> >> >>"
  -- Radio group: /V / means empty-name selection (unselected)
  , "<< /FT /Btn /Ff 49152 /T (RadioGroup) /V / /Kids [ 7 0 R 8 0 R ] >>"
  -- Widget annotations without /T (anonymous)
  , "<< /Type /Annot /Subtype /Widget /Parent 6 0 R /Rect [100 700 120 720] /P 3 0 R /AS /Off >>"
  , "<< /Type /Annot /Subtype /Widget /Parent 6 0 R /Rect [150 700 170 720] /P 3 0 R /AS /Off >>"
  -- Text field
  , "<< /Type /Annot /Subtype /Widget /FT /Tx /T (TextField1) /V (InitVal) /Rect [100 650 400 670] /P 3 0 R /DA (/Helv 12 Tf 0 g) >>"
  ]

-- ---------------------------------------------------------------------------
-- Test runner

type FailRef = IORef [String]

assertM :: FailRef -> String -> Bool -> IO ()
assertM ref msg ok = unless ok $ modifyIORef ref (msg :)

runTest :: FailRef -> String -> IO () -> IO ()
runTest failRef testName action = do
  prevFails <- readIORef failRef
  action
  newFails <- readIORef failRef
  if length newFails == length prevFails
    then putStrLn $ "  PASS  " <> testName
    else do
      let newMsgs = take (length newFails - length prevFails) newFails
      putStrLn $ "  FAIL  " <> testName
      mapM_ (\m -> putStrLn $ "        " <> m) (reverse newMsgs)

-- | Build a minimal FDF value to use as fill input.
makeFillFDF :: Field -> FDF
makeFillFDF f = FDF "1 0 obj\n" f "endobj\ntrailer\n\n<<\n/Root 1 0 R\n>>\n"

-- ---------------------------------------------------------------------------
-- Tests

-- | Parsing simplePDF should produce a single text field with value "Hello".
testParseSimple :: FailRef -> IO ()
testParseSimple ref =
  case parsePDF simplePDF of
    Left err  -> modifyIORef ref (("parsePDF simplePDF: " <> err) :)
    Right fdf -> do
      assertM ref "simplePDF: field name should be TextField1" $
        name (body fdf) == "TextField1"
      assertM ref "simplePDF: field value should be Hello" $
        content (body fdf) == FieldValue "Hello"

-- | Parsing radioPDF should produce a radio group field (with /V /)
-- and a text field.
testParseRadio :: FailRef -> IO ()
testParseRadio ref =
  case parsePDF radioPDF of
    Left err  -> modifyIORef ref (("parsePDF radioPDF: " <> err) :)
    Right fdf ->
      case content (body fdf) of
        Children kids -> do
          assertM ref ("Expected 2 top-level children, got " <> show (length kids))
                      (length kids == 2)
          case kids of
            [radioField, textField] -> do
              assertM ref "radio group name" (name radioField == "RadioGroup")
              assertM ref "radio group /V /" (content radioField == FieldNameValue "")
              assertM ref "text field name" (name textField == "TextField1")
              assertM ref "text field /V (InitVal)" (content textField == FieldValue "InitVal")
            _ -> return ()
        other ->
          modifyIORef ref (("Expected Children, got: " <> show other) :)

-- | fillPDF should update a text field value and parsePDF on the result
-- should return the new value.
testFillSimple :: FailRef -> IO ()
testFillSimple ref =
  let fdf = makeFillFDF Field { name = "TextField1", content = FieldValue "World" }
  in case fillPDF fdf simplePDF of
       Left err     -> modifyIORef ref (("fillPDF simplePDF: " <> err) :)
       Right filled ->
         case parsePDF filled of
           Left err   -> modifyIORef ref (("round-trip parse: " <> err) :)
           Right fdf2 ->
             assertM ref "filled value should be 'World'" $
               content (body fdf2) == FieldValue "World"

-- | fillPDF on a radio-button PDF should allow updating the text field
-- and the result should round-trip correctly.
testFillRadio :: FailRef -> IO ()
testFillRadio ref =
  let fdf = makeFillFDF Field { name = "TextField1", content = FieldValue "Filled" }
  in case fillPDF fdf radioPDF of
       Left err     -> modifyIORef ref (("fillPDF radioPDF: " <> err) :)
       Right filled ->
         case parsePDF filled of
           Left err   -> modifyIORef ref (("round-trip parse: " <> err) :)
           Right fdf2 ->
             case content (body fdf2) of
               Children kids ->
                 let textFields = filter (\k -> name k == "TextField1") kids
                 in assertM ref "TextField1 should be 'Filled'" $
                      all (\k -> content k == FieldValue "Filled") textFields
               FieldValue v ->
                 assertM ref "single field should be 'Filled'" (v == "Filled")
               other ->
                 modifyIORef ref (("Unexpected body: " <> show other) :)

-- | The empty-name radio button value @\/V \/@ must survive a fill-and-parse
-- round-trip (regression for the \"Empty name\" / \"Unexpected character: e\" bug).
testEmptyNameRoundTrip :: FailRef -> IO ()
testEmptyNameRoundTrip ref =
  let fdf = makeFillFDF Field { name = "TextField1", content = FieldValue "X" }
  in case fillPDF fdf radioPDF of
       Left err     -> modifyIORef ref (("fillPDF radioPDF: " <> err) :)
       Right filled ->
         case parsePDF filled of
           Left err   -> modifyIORef ref (("round-trip parse: " <> err) :)
           Right fdf2 ->
             case content (body fdf2) of
               Children kids ->
                 let radioKids = filter (\k -> name k == "RadioGroup") kids
                 in assertM ref "radio /V / should survive round-trip" $
                      all (\k -> content k == FieldNameValue "") radioKids
               other ->
                 modifyIORef ref (("Unexpected body: " <> show other) :)

-- | A fill-and-parse round-trip on a PDF whose field annotation has
-- fractional /Rect coordinates (e.g. 0.05) whose @show@ representation
-- includes an @e@ exponent.  Before the fix this caused
-- \"Unexpected character: e\" when pdf-to-fdf read the filled PDF.
testFloatRectRoundTrip :: FailRef -> IO ()
testFloatRectRoundTrip ref =
  let fdf = makeFillFDF Field { name = "FloatField", content = FieldValue "OK" }
  in case fillPDF fdf floatRectPDF of
       Left err     -> modifyIORef ref (("fillPDF floatRectPDF: " <> err) :)
       Right filled ->
         case parsePDF filled of
           Left err   -> modifyIORef ref (("round-trip parse (floatRect): " <> err) :)
           Right fdf2 ->
             assertM ref "FloatField value should be 'OK'" $
               content (body fdf2) == FieldValue "OK"

-- ---------------------------------------------------------------------------
-- Main

main :: IO ()
main = do
  failRef <- newIORef []
  let run name t = runTest failRef name (t failRef)
  run "parse simple text field PDF"          testParseSimple
  run "parse radio button PDF"               testParseRadio
  run "fill simple text field"               testFillSimple
  run "fill radio PDF (text field only)"     testFillRadio
  run "empty-name /V / survives round-trip"  testEmptyNameRoundTrip
  run "float /Rect coords survive round-trip" testFloatRectRoundTrip
  failures <- readIORef failRef
  if null failures
    then do
      putStrLn "\nAll tests passed."
      exitSuccess
    else do
      putStrLn $ "\n" <> show (length failures) <> " assertion(s) failed."
      exitFailure
