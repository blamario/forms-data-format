{-# LANGUAGE OverloadedStrings #-}

-- | Extract text labels from PDF page content streams and associate them
-- with form fields by proximity.
--
-- This module contains the low-level machinery for 'Text.FDF.PDF.fieldLabels':
-- field bounding-box collection, page text loading, and proximity matching.

module Text.FDF.PDF.Labels (
  buildFieldLabels,
) where

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.List (nub)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text

import Text.FDF (Field (..), FieldContent (..))
import Text.FDF.PDF.ContentStream (TextFragment (..), extractTextFragments)
import Text.FDF.PDF.Types

-- ---------------------------------------------------------------------------
-- Loader function types
--
-- These function types abstract over the PDF object-loading machinery so that
-- this module can be compiled without depending on the top-level 'PDF' module.

-- | Load and dereference a PDF object.
type ObjLoader = PDFValue -> Either String PDFValue

-- | Load a page's decompressed content-stream bytes by its object number.
type PageStreamLoader = Int -> Either String ByteString

-- ---------------------------------------------------------------------------
-- Public API

-- | Build a list of 'Field's mirroring the AcroForm hierarchy where each
-- leaf field's value is the nearby page text (its label).  The hierarchy is
-- preserved: parent fields with named children produce 'Children' nodes.
--
-- Fields without a @\/Rect@ or @\/P@ (and thus without locatable labels)
-- are included with an empty 'FieldValue'.
buildFieldLabels
  :: ObjLoader
  -> PageStreamLoader
  -> [PDFValue]           -- ^ the AcroForm @\/Fields@ array entries
  -> Either String [Field]
buildFieldLabels loadObj loadPage fieldsArr = do
  -- First pass: collect all page object numbers so we can batch
  -- page-content loading.
  allPages <- collectAllPages loadObj [] fieldsArr
  let pageObjNums = nub allPages
  -- Load page text fragments, keyed by page object number.
  pageTexts <- mapM (\p -> (,) p <$> loadPageFragments loadPage p) pageObjNums
  let pageTextMap = Map.fromList pageTexts
  -- Second pass: build the Field tree with labels.
  buildLabelFields loadObj pageTextMap [] fieldsArr

-- ---------------------------------------------------------------------------
-- Field hierarchy building

-- | Walk the AcroForm field hierarchy and produce a list of 'Field's where
-- leaf values are the nearby text labels.
buildLabelFields
  :: ObjLoader
  -> Map Int [TextFragment]    -- ^ page object number → text fragments
  -> [Text]                    -- ^ path prefix (ancestor names)
  -> [PDFValue]                -- ^ field references
  -> Either String [Field]
buildLabelFields loadObj pageTextMap prefix refs = do
  mFields <- mapM (buildLabelField loadObj pageTextMap prefix) refs
  Right [f | Just f <- mFields]

buildLabelField
  :: ObjLoader
  -> Map Int [TextFragment]
  -> [Text]
  -> PDFValue
  -> Either String (Maybe Field)
buildLabelField loadObj pageTextMap prefix ref = do
  obj <- loadObj ref
  case obj of
    PDFDict dict -> buildFromDict loadObj pageTextMap prefix dict
    _            -> Right Nothing

-- | Build a 'Field' from a field dictionary.  Returns 'Nothing' for widget
-- annotations without a @\/T@ entry (anonymous widgets).
buildFromDict
  :: ObjLoader
  -> Map Int [TextFragment]
  -> [Text]
  -> Map ByteString PDFValue
  -> Either String (Maybe Field)
buildFromDict loadObj pageTextMap prefix dict =
  case Map.lookup "T" dict of
    Nothing -> Right Nothing   -- anonymous widget → skip
    _ -> do
      fieldName <- decodeFieldText dict "T"
      let path = prefix ++ [fieldName]
      cont <- case Map.lookup "Kids" dict of
        Just (PDFArray kids) -> do
          childFields <- buildLabelFields loadObj pageTextMap path kids
          if null childFields
            then leafLabel pageTextMap dict  -- all kids are widgets → leaf
            else Right (Children childFields)
        Just kidRef@PDFRef{} -> do
          kidsVal <- loadObj kidRef
          case kidsVal of
            PDFArray kids -> do
              childFields <- buildLabelFields loadObj pageTextMap path kids
              if null childFields
                then leafLabel pageTextMap dict
                else Right (Children childFields)
            _ -> leafLabel pageTextMap dict
        _ -> leafLabel pageTextMap dict
      Right $ Just Field { name = fieldName, content = cont }

-- | Produce a leaf 'FieldContent' whose value is the concatenation of
-- nearby text fragments.
leafLabel :: Map Int [TextFragment] -> Map ByteString PDFValue -> Either String FieldContent
leafLabel pageTextMap dict =
  case extractRectAndPage dict of
    Just (pageNum, llx, lly, urx, ury) ->
      let frags = fromMaybe [] (Map.lookup pageNum pageTextMap)
          nearby = [fragmentText f | f <- frags, isNearby llx lly urx ury f]
      in Right $ FieldValue (Text.intercalate " " nearby)
    Nothing -> Right (FieldValue "")

-- ---------------------------------------------------------------------------
-- Page number collection

-- | Collect all page object numbers referenced by fields in the hierarchy
-- (for determining which pages need their content streams loaded).
collectAllPages
  :: ObjLoader
  -> [Text]        -- ^ path prefix
  -> [PDFValue]    -- ^ field / widget references
  -> Either String [Int]
collectAllPages loadObj prefix refs =
  concat <$> mapM (collectPageNums loadObj prefix) refs

collectPageNums
  :: ObjLoader
  -> [Text]
  -> PDFValue
  -> Either String [Int]
collectPageNums loadObj prefix ref = do
  obj <- loadObj ref
  case obj of
    PDFDict dict -> do
      let path = case Map.lookup "T" dict of
                   Just (PDFString s) -> prefix ++ [decodePDFString s]
                   Just (PDFName nm)  -> prefix ++ [Text.decodeLatin1 nm]
                   _                  -> prefix
      case Map.lookup "Kids" dict of
        Just (PDFArray kids) -> collectAllPages loadObj path kids
        Just kidRef@PDFRef{} -> do
          kidsVal <- loadObj kidRef
          case kidsVal of
            PDFArray kids -> collectAllPages loadObj path kids
            _             -> pageFromDict dict
        _                    -> pageFromDict dict
    _ -> Right []

-- | Extract the page object number from a field/widget dictionary.
pageFromDict :: Map ByteString PDFValue -> Either String [Int]
pageFromDict dict =
  case Map.lookup "P" dict of
    Just (PDFRef pn _) -> Right [pn]
    _                  -> Right []

-- | Extract the @\/Rect@ and @\/P@ from a dictionary, if both are present.
extractRectAndPage :: Map ByteString PDFValue -> Maybe (Int, Double, Double, Double, Double)
extractRectAndPage dict =
  case (Map.lookup "Rect" dict, Map.lookup "P" dict) of
    (Just (PDFArray [a, b, c, d]), Just (PDFRef pn _)) ->
      case (toDouble a, toDouble b, toDouble c, toDouble d) of
        (Just llx, Just lly, Just urx, Just ury) -> Just (pn, llx, lly, urx, ury)
        _                                        -> Nothing
    _ -> Nothing

-- ---------------------------------------------------------------------------
-- Proximity matching

-- | Proximity margin in PDF points (1 pt = 1\/72 inch).  Text fragments
-- within this distance of a field's bounding box are considered nearby
-- labels.  60 pt ≈ 0.83 in — generous enough to catch labels placed above,
-- below, or to the left\/right of typical form fields.
proximityMargin :: Double
proximityMargin = 60

-- | Determine whether a 'TextFragment' is "nearby" a given bounding box.
isNearby :: Double -> Double -> Double -> Double -> TextFragment -> Bool
isNearby llx lly urx ury tf =
  let tx = fragmentX tf
      ty = fragmentY tf
      inXRange = tx >= llx - proximityMargin && tx <= urx + proximityMargin
      inYRange = ty >= lly - proximityMargin && ty <= ury + proximityMargin
  in inXRange && inYRange && not (Text.null (Text.strip (fragmentText tf)))

-- ---------------------------------------------------------------------------
-- Page content stream loading

-- | Load page text fragments via the page-stream loader.
loadPageFragments :: PageStreamLoader -> Int -> Either String [TextFragment]
loadPageFragments loadPage pageObjNum = do
  contentsBytes <- loadPage pageObjNum
  Right (extractTextFragments contentsBytes)

-- ---------------------------------------------------------------------------
-- Helpers

-- | Decode the value of a string-typed field entry as 'Text'.
decodeFieldText :: Map ByteString PDFValue -> ByteString -> Either String Text
decodeFieldText dict key =
  case Map.lookup key dict of
    Nothing             -> Left ("Field is missing /" <> show key)
    Just (PDFString bs) -> Right (decodePDFString bs)
    Just (PDFName nm)   -> Right (Text.decodeLatin1 nm)
    Just v              -> Left ("/" <> show key <> " has unexpected type: " <> show v)

-- | Decode a raw PDF string to 'Text'.
decodePDFString :: ByteString -> Text
decodePDFString bs
  | "\xFE\xFF" `BS.isPrefixOf` bs = Text.decodeUtf16BE (BS.drop 2 bs)
  | otherwise                      = Text.decodeLatin1 bs

-- | Convert a 'PDFValue' to 'Double'.
toDouble :: PDFValue -> Maybe Double
toDouble (PDFInt n)  = Just (fromIntegral n)
toDouble (PDFReal r) = Just r
toDouble _           = Nothing


