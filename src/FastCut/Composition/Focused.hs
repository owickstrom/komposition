{-# LANGUAGE GADTs      #-}
{-# LANGUAGE LambdaCase #-}
-- | Annotate a composition with focus metadata.

module FastCut.Composition.Focused where

import           FastCut.Prelude

import           Data.List.NonEmpty   (NonEmpty (..))
import qualified Data.List.NonEmpty   as NonEmpty

import           FastCut.Composition
import           FastCut.Focus
import           FastCut.MediaType

data Focused
  = Focused
  | TransitivelyFocused
  | Blurred
  deriving (Eq, Show)

blurComposition :: Composition a t -> Composition Focused t
blurComposition = \case
  Timeline _ sub -> Timeline Blurred (map blurComposition sub)
  Sequence _ sub -> Sequence Blurred (map blurComposition sub)
  Parallel _ videoParts audioParts ->
    Parallel Blurred (map blurPart videoParts) (map blurPart audioParts)

blurPart :: CompositionPart a t -> CompositionPart Focused t
blurPart = setPartAnnotation Blurred

numsFromZero :: (Enum n, Num n) => NonEmpty n
numsFromZero = 0 :| [1 ..]

applyFocus :: Composition a t -> Focus ft -> Composition Focused t
applyFocus = go
  where
    go :: Composition a t -> Focus ft -> Composition Focused t
    go (Timeline _ sub) (SequenceFocus idx parallelFocus) =
      Timeline
        TransitivelyFocused
        (NonEmpty.zipWith (applyAtSubComposition idx parallelFocus) sub numsFromZero)
    go (Sequence _ sub) (ParallelFocus idx subFocus) =
      Sequence
        TransitivelyFocused
        (NonEmpty.zipWith (applyAtSubComposition idx subFocus) sub numsFromZero)
    go (Parallel _ videoParts audioParts) (ClipFocus focusPartType idx) =
      let focusInParts :: [CompositionPart a mt] -> [CompositionPart Focused mt]
          focusInParts clips = zipWith (focusPartAt idx) clips [0 ..]
          (videoParts', audioParts') =
            case focusPartType of
              Video -> (focusInParts videoParts, map blurPart audioParts)
              Audio -> (map blurPart videoParts, focusInParts audioParts)
      in Parallel TransitivelyFocused videoParts' audioParts'
    go c _ = blurComposition c
  -- Apply focus at the sub-composition specified by 'idx'.
    applyAtSubComposition ::
         Int
      -> Maybe (Focus ft)
      -> Composition a t
      -> Int
      -> Composition Focused t
    applyAtSubComposition idx subFocus subComposition subIdx
      | subIdx == idx =
        case subFocus of
          Just focus ->
            go
              (setCompositionAnnotation
                 TransitivelyFocused
                 (blurComposition subComposition))
              focus
          Nothing ->
            setCompositionAnnotation Focused (blurComposition subComposition)
      | otherwise = blurComposition subComposition
  -- Apply focus at the clip specified by 'idx'.
    focusPartAt :: Int -> CompositionPart a mt -> Int -> CompositionPart Focused mt
    focusPartAt idx clip clipIdx
      | clipIdx == idx = setPartAnnotation Focused clip
      | otherwise = blurPart clip
