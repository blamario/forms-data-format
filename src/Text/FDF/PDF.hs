{-# LANGUAGE OverloadedStrings #-}

-- | Conversion between PDF AcroForm fields and FDF format.
--
-- References: PDF 32000-1:2008 (PDF 1.7 specification):
--
-- * §7.3 – Objects (booleans, numbers, strings, names, arrays, dictionaries)
-- * §7.5 – File structure (cross-reference tables and streams, incremental updates)
-- * §7.6 – Encryption (Standard Security Handler, AES-128-CBC, per-object keys)
-- * §12.7 – Interactive forms (AcroForm, field dictionaries, @\/Kids@, @\/T@, @\/V@)
--
-- Low-level parsing helpers live in "Text.FDF.PDF.Internal".
module Text.FDF.PDF
  ( parsePDF
  , fillPDF
  ) where

import Data.ByteString (ByteString)
import Data.List (foldl')
import qualified Data.Map.Strict as Map
import Data.Maybe (catMaybes, fromMaybe)

import Text.FDF (FDF (..), Field (..), FieldContent (..))
import Text.FDF.PDF.Internal
  ( PDFValue (PDFInt)
  , appendIncrementalUpdate
  , applyUpdate
  , buildPathMap
  , collectUpdates
  , loadAcroFormFields
  , loadFieldObj
  )

-- | Extract form field data from a PDF file.
--
-- Reads the PDF's AcroForm structure and returns the corresponding 'FDF'
-- value.  Supports both traditional (table-based) cross-reference sections
-- and cross-reference streams (PDF 1.5+), including FlateDecode-compressed
-- object streams.
parsePDF :: ByteString -> Either String FDF
parsePDF bs = do
  (_, xref, trailer, dec, _enc, fieldsArr) <- loadAcroFormFields bs
  fields <- catMaybes <$> mapM (loadFieldObj bs xref dec) fieldsArr
  case fields of
    []  -> Left "PDF has no AcroForm fields"
    [f] -> Right $ FDF
             "1 0 obj\n"
             f
             "endobj\ntrailer\n\n<<\n/Root 1 0 R\n>>\n"
    _   -> Right $ FDF
             "1 0 obj\n"
             Field { name = "", content = Children fields }
             "endobj\ntrailer\n\n<<\n/Root 1 0 R\n>>\n"

-- | Fill the form fields of a PDF template using an 'FDF' value.
--
-- Uses an incremental-update append so the original PDF bytes are left intact.
-- For encrypted PDFs (Standard Security Handler, empty user password), the
-- new field-value objects are AES-encrypted with the same key as the original
-- body; the incremental update trailer carries both @\/Encrypt@ and @\/ID@ so
-- readers can decrypt both the original body objects and the new field objects.
fillPDF :: FDF -> ByteString -> Either String ByteString
fillPDF fdf pdfBytes = do
  (xrefOff, xref, trailer, dec, enc, fieldsArr) <- loadAcroFormFields pdfBytes
  -- Build mapping:  full path → (objNum, current dict)
  pathMap      <- buildPathMap pdfBytes xref dec [] fieldsArr
  -- Collect leaf-value updates from FDF
  let updates   = collectUpdates [] (body fdf)
  -- Apply updates: produce list of (objNum, new dict)
  let totalObjs = fromMaybe 0 $ do
        PDFInt n <- Map.lookup "Size" trailer
        return n
  (newObjs, _) <- foldl' (applyUpdate pathMap) (Right ([], totalObjs)) updates
  Right $ if null newObjs
    then pdfBytes
    else appendIncrementalUpdate enc pdfBytes xrefOff trailer newObjs

