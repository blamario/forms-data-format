{-# LANGUAGE OverloadedStrings #-}

-- | Extract text labels from PDF page content streams and associate them
-- with form fields by proximity.
--
-- This module contains the low-level machinery for 'Text.FDF.PDF.fieldLabels':
-- field bounding-box collection, page text loading, and proximity matching.
--
-- Each text fragment is assigned to at most one field — the nearest one
-- within the proximity margin — so that overlapping expanded bounding
-- rectangles do not cause the same label to appear in multiple fields.

module Text.FDF.PDF.Labels (
  buildFieldLabels,
  LabelConfig (..),
  SearchZone (..),
  defaultLabelConfig,
) where

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.List (minimumBy, nub)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe, maybeToList)
import Data.Ord (comparing)
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
-- Configuration types

-- | Which spatial zones around a field box to search for label text.
-- Each margin is measured in PDF points (1 pt = 1\/72 inch).
data SearchZone = SearchZone
  { zoneLeft   :: !Double   -- ^ margin to the left of the box
  , zoneRight  :: !Double   -- ^ margin to the right of the box
  , zoneAbove  :: !Double   -- ^ margin above the box
  , zoneBelow  :: !Double   -- ^ margin below the box
  } deriving (Eq, Show)

-- | Configuration controlling how text fragments are matched to fields.
--
-- Use 'defaultLabelConfig' for the standard 60 pt uniform margin.
-- For form-specific matching, adjust the 'SearchZone' margins.
-- For example, Canadian income tax forms have relevant text in the same
-- horizontal line as the field box — use a wide left margin with a tight
-- vertical margin.  To obtain different /kinds/ of labels (e.g. descriptions
-- vs. line numbers) call 'Text.FDF.PDF.fieldLabelsWith' multiple times with
-- different configurations.
data LabelConfig = LabelConfig
  { lcSearchZone  :: SearchZone
    -- ^ Asymmetric proximity margins around each field box.
  , lcSeparator   :: Text
    -- ^ Separator used when concatenating fragment texts into a label value.
    -- Default: @\" \"@ (single space).
  } deriving (Eq, Show)

-- | Default configuration matching the original behaviour:
-- 60 pt margin in all directions, fragments joined by a single space.
defaultLabelConfig :: LabelConfig
defaultLabelConfig = LabelConfig
  { lcSearchZone = SearchZone 60 60 60 60
  , lcSeparator  = " "
  }

-- ---------------------------------------------------------------------------
-- Rectangle type

-- | A PDF bounding rectangle with lower-left and upper-right corners.
data Rect = Rect
  { rectLLX :: !Double   -- ^ lower-left X
  , rectLLY :: !Double   -- ^ lower-left Y
  , rectURX :: !Double   -- ^ upper-right X
  , rectURY :: !Double   -- ^ upper-right Y
  } deriving (Eq, Ord, Show)

-- | A field bounding rectangle: page object number paired with the rect.
type FieldRect = (Int, Rect)

-- ---------------------------------------------------------------------------
-- Public API

-- | Build a list of 'Field's mirroring the AcroForm hierarchy where each
-- leaf field's value is the nearby page text (its label).  The hierarchy is
-- preserved: parent fields with named children produce 'Children' nodes.
--
-- Each text fragment is assigned to at most one field — the one whose
-- bounding box is nearest — so overlapping proximity zones do not cause a
-- label to appear in multiple fields.
--
-- Fields without a @\/Rect@ or @\/P@ (and thus without locatable labels)
-- are included with an empty 'FieldValue'.
--
-- See 'Text.FDF.PDF.fieldLabelsWith' for the public entry point.
-- To obtain different kinds of labels (e.g. descriptions vs. line numbers),
-- call 'Text.FDF.PDF.fieldLabelsWith' multiple times with different
-- 'LabelConfig' values.
buildFieldLabels
  :: LabelConfig
  -> ObjLoader
  -> PageStreamLoader
  -> [PDFValue]           -- ^ the AcroForm @\/Fields@ array entries
  -> Either String [Field]
buildFieldLabels config loadObj loadPage fieldsArr = do
  -- First pass: collect all page object numbers so we can batch
  -- page-content loading.
  allPages <- collectAllPages loadObj [] fieldsArr
  let pageObjNums = nub allPages
  -- Load page text fragments, keyed by page object number.
  pageTexts <- mapM (\p -> (,) p <$> loadPageFragments loadPage p) pageObjNums
  let pageTextMap = Map.fromList pageTexts
  -- Collect all leaf field bounding rectangles.
  allRects <- collectLeafRects loadObj fieldsArr
  -- Assign each text fragment exclusively to the nearest field.
  let assignments = assignExclusively config pageTextMap allRects
  -- Second pass: build the Field tree with labels.
  buildLabelFields config loadObj assignments [] fieldsArr

-- ---------------------------------------------------------------------------
-- Field hierarchy building

-- | Walk the AcroForm field hierarchy and produce a list of 'Field's where
-- leaf values are the nearby text labels.
buildLabelFields
  :: LabelConfig
  -> ObjLoader
  -> Map FieldRect [Text]      -- ^ exclusive text assignments
  -> [Text]                    -- ^ path prefix (ancestor names)
  -> [PDFValue]                -- ^ field references
  -> Either String [Field]
buildLabelFields config loadObj assignments prefix refs = do
  mFields <- mapM (buildLabelField config loadObj assignments prefix) refs
  Right [f | Just f <- mFields]

buildLabelField
  :: LabelConfig
  -> ObjLoader
  -> Map FieldRect [Text]
  -> [Text]
  -> PDFValue
  -> Either String (Maybe Field)
buildLabelField config loadObj assignments prefix ref = do
  obj <- loadObj ref
  case obj of
    PDFDict dict -> buildFromDict config loadObj assignments prefix dict
    _            -> Right Nothing

-- | Build a 'Field' from a field dictionary.  Returns 'Nothing' for widget
-- annotations without a @\/T@ entry (anonymous widgets).
buildFromDict
  :: LabelConfig
  -> ObjLoader
  -> Map FieldRect [Text]
  -> [Text]
  -> Map ByteString PDFValue
  -> Either String (Maybe Field)
buildFromDict config loadObj assignments prefix dict =
  case Map.lookup "T" dict of
    Nothing -> Right Nothing   -- anonymous widget → skip
    _ -> do
      fieldName <- decodeFieldText dict "T"
      let path = prefix ++ [fieldName]
      cont <- case Map.lookup "Kids" dict of
        Just (PDFArray kids) -> do
          childFields <- buildLabelFields config loadObj assignments path kids
          if null childFields
            then leafLabel config assignments dict  -- all kids are widgets → leaf
            else Right (Children childFields)
        Just kidRef@PDFRef{} -> do
          kidsVal <- loadObj kidRef
          case kidsVal of
            PDFArray kids -> do
              childFields <- buildLabelFields config loadObj assignments path kids
              if null childFields
                then leafLabel config assignments dict
                else Right (Children childFields)
            _ -> leafLabel config assignments dict
        _ -> leafLabel config assignments dict
      Right $ Just Field { name = fieldName, content = cont }

-- | Produce a leaf 'FieldContent' whose value is the concatenation of
-- text fragments exclusively assigned to this field.
leafLabel :: LabelConfig -> Map FieldRect [Text] -> Map ByteString PDFValue -> Either String FieldContent
leafLabel config assignments dict =
  case extractRectAndPage dict of
    Just rect ->
      let nearby = fromMaybe [] (Map.lookup rect assignments)
      in Right $ FieldValue (Text.intercalate (lcSeparator config) nearby)
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
extractRectAndPage :: Map ByteString PDFValue -> Maybe FieldRect
extractRectAndPage dict =
  case (Map.lookup "Rect" dict, Map.lookup "P" dict) of
    (Just (PDFArray [a, b, c, d]), Just (PDFRef pn _)) ->
      case (toDouble a, toDouble b, toDouble c, toDouble d) of
        (Just llx, Just lly, Just urx, Just ury) -> Just (pn, Rect llx lly urx ury)
        _                                        -> Nothing
    _ -> Nothing

-- ---------------------------------------------------------------------------
-- Leaf field rectangle collection

-- | Walk the AcroForm field hierarchy and collect the bounding rectangle
-- of every leaf field (fields that will receive a label).
collectLeafRects :: ObjLoader -> [PDFValue] -> Either String [FieldRect]
collectLeafRects loadObj refs = concat <$> mapM (collectLeafRect loadObj) refs

collectLeafRect :: ObjLoader -> PDFValue -> Either String [FieldRect]
collectLeafRect loadObj ref = do
  obj <- loadObj ref
  case obj of
    PDFDict dict ->
      case Map.lookup "T" dict of
        Nothing -> Right []   -- anonymous widget
        Just _  -> collectLeafRectFromDict loadObj dict
    _ -> Right []

collectLeafRectFromDict :: ObjLoader -> Map ByteString PDFValue -> Either String [FieldRect]
collectLeafRectFromDict loadObj dict =
  case Map.lookup "Kids" dict of
    Just (PDFArray kids) -> do
      named <- anyHasFieldName loadObj kids
      if named
        then collectLeafRects loadObj kids
        else Right ownRect
    Just kidRef@PDFRef{} -> do
      kidsVal <- loadObj kidRef
      case kidsVal of
        PDFArray kids -> do
          named <- anyHasFieldName loadObj kids
          if named
            then collectLeafRects loadObj kids
            else Right ownRect
        _ -> Right ownRect
    _ -> Right ownRect
  where ownRect = maybeToList (extractRectAndPage dict)

-- | Check whether any of the given field references is a named field
-- (has a @\/T@ entry), as opposed to an anonymous widget annotation.
anyHasFieldName :: ObjLoader -> [PDFValue] -> Either String Bool
anyHasFieldName loadObj = fmap or . mapM check
  where
    check ref = do
      obj <- loadObj ref
      case obj of
        PDFDict dict -> Right (Map.member "T" dict)
        _            -> Right False

-- ---------------------------------------------------------------------------
-- Exclusive text assignment

-- | Assign each text fragment to at most one field — the one whose
-- bounding box is nearest.  This prevents overlapping proximity zones
-- from causing the same label to appear under multiple fields.
assignExclusively :: LabelConfig -> Map Int [TextFragment] -> [FieldRect] -> Map FieldRect [Text]
assignExclusively config pageTextMap allRects =
  let sz = lcSearchZone config
      rectsByPage :: Map Int [Rect]
      rectsByPage = Map.fromListWith (++)
        [(p, [r]) | (p, r) <- allRects]
      assignPage pageNum frags =
        [ ((pageNum, nr), fragmentText f)
        | f <- frags
        , not (Text.null (Text.strip (fragmentText f)))
        , nr <- maybeToList
            (nearestRect sz (fromMaybe [] (Map.lookup pageNum rectsByPage)) f)
        ]
      allAssignments = concatMap
        (\(pn, frags) -> assignPage pn frags)
        (Map.toList pageTextMap)
  in Map.fromListWith (++)
       [(rect, [txt]) | (rect, txt) <- allAssignments]

-- | Find the nearest field rectangle to a text fragment, considering only
-- rectangles within the search zone.  Returns 'Nothing' if no
-- rectangle is close enough.
nearestRect :: SearchZone -> [Rect] -> TextFragment -> Maybe Rect
nearestRect sz rects tf =
  case [(r, rectDist r tf) | r <- rects, isNearbyRect sz r tf] of
    []         -> Nothing
    candidates -> Just (fst (minimumBy (comparing snd) candidates))

-- | Check whether a text fragment is within the search zone of a
-- rectangle.
isNearbyRect :: SearchZone -> Rect -> TextFragment -> Bool
isNearbyRect sz (Rect llx lly urx ury) = isNearby sz llx lly urx ury

-- | Squared distance from a text fragment's position to the nearest edge
-- of a rectangle.  Returns 0 when the point lies inside the rectangle.
rectDist :: Rect -> TextFragment -> Double
rectDist (Rect llx lly urx ury) tf =
  let tx = fragmentX tf
      ty = fragmentY tf
      dx = max 0 (max (llx - tx) (tx - urx))
      dy = max 0 (max (lly - ty) (ty - ury))
  in dx * dx + dy * dy

-- ---------------------------------------------------------------------------
-- Proximity matching

-- | Determine whether a 'TextFragment' is within the given 'SearchZone'
-- of a bounding box.
isNearby :: SearchZone -> Double -> Double -> Double -> Double -> TextFragment -> Bool
isNearby sz llx lly urx ury tf =
  let tx = fragmentX tf
      ty = fragmentY tf
      inXRange = tx >= llx - zoneLeft sz && tx <= urx + zoneRight sz
      inYRange = ty >= lly - zoneBelow sz && ty <= ury + zoneAbove sz
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


