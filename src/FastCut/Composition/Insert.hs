{-# OPTIONS_GHC -fno-warn-unticked-promoted-constructors #-}
{-# LANGUAGE DataKinds  #-}
{-# LANGUAGE GADTs      #-}
{-# LANGUAGE RankNTypes #-}

-- | Transform a 'Composition' by inserting children.
module FastCut.Composition.Insert where

import           FastCut.Prelude

import qualified Data.List.NonEmpty   as NonEmpty

import           FastCut.Composition
import           FastCut.Focus
import           FastCut.Focus.Parent
import           FastCut.MediaType

spliceAt :: Int -> [a] -> [a] -> [a]
spliceAt i new xs =
  let (before, after) = splitAt i xs
  in before <> new <> after

insertAtNonEmpty :: Int -> a -> NonEmpty a -> NonEmpty a
insertAtNonEmpty i x xs =
  let (before, after) = NonEmpty.splitAt i xs
  in  NonEmpty.fromList (before <> (x : after))

spliceAtPositionInList
  :: Applicative f => InsertPosition -> Int -> [a] -> [a] -> f [a]
spliceAtPositionInList position i new xs = case position of
  LeftMost  -> pure (new <> xs)
  LeftOf    -> pure (spliceAt i new xs)
  RightOf   -> pure (spliceAt (succ i) new xs)
  RightMost -> pure (xs <> new)

insertAtPositionInNonEmpty
  :: Applicative f => InsertPosition -> Int -> NonEmpty a -> a -> f (NonEmpty a)
insertAtPositionInNonEmpty position i xs x = case position of
  LeftMost  -> pure (pure x <> xs)
  LeftOf    -> pure (insertAtNonEmpty i x xs)
  RightOf   -> pure (insertAtNonEmpty (succ i) x xs)
  RightMost -> pure (xs <> pure x)


data InsertPosition
  = LeftMost
  | LeftOf
  | RightOf
  | RightMost
  deriving (Show, Eq, Ord, Enum, Bounded)

data Insertion a
  = InsertSequence (Composition SequenceType a)
  | InsertParallel (Composition ParallelType a)
  | InsertVideoParts [CompositionPart Video a]
  | InsertAudioParts [CompositionPart Audio a]
  deriving (Show, Eq)

-- | Inserts a 'Composition' or 'CompositionPart', wrapped in the
-- 'Insertion', relative to 'InsertPosition' and the 'Focus'.
insert
  :: Focus ft
  -> Insertion a
  -> InsertPosition
  -> Composition TimelineType a
  -> Maybe (Composition TimelineType a)
insert focus insertion position parent =
  case (insertion, position, focusType focus) of
    (InsertSequence sequence', _, SequenceFocusType) ->
      traverseWithParent
        (parentTraversal
         { onTimeline =
             \i (Timeline children') ->
               Timeline <$>
               insertAtPositionInNonEmpty position i children' sequence'
         })
    (InsertParallel parallel, _, _) ->
      traverseWithParent
        (parentTraversal
         { onSequence =
             \i (Sequence ann children') ->
               Sequence ann <$>
               insertAtPositionInNonEmpty position i children' parallel
         })
    (InsertVideoParts clips, _, ClipFocusType) ->
      traverseWithParent
        (parentTraversal
         {onVideoParts = \i vs -> spliceAtPositionInList position i clips vs})
    (InsertAudioParts clips, _, ClipFocusType) ->
      traverseWithParent
        (parentTraversal
         {onAudioParts = \i as -> spliceAtPositionInList position i clips as})
    (InsertVideoParts clips, _, ParallelFocusType) ->
      let traversal =
            FocusedTraversal
            { mapSequence = pure
            , mapParallel =
                \(Parallel ann videoParts audioParts) ->
                  pure
                    (Parallel
                       ann
                       (spliceLeftMostOrRightMost position clips videoParts)
                       audioParts)
            , mapCompositionPart = const pure
            }
      in mapAtFocus focus traversal parent
    (InsertAudioParts clips, _, ParallelFocusType) ->
      let traversal =
            FocusedTraversal
            { mapSequence = pure
            , mapParallel =
                \(Parallel ann videoParts audioParts) ->
                  pure
                    (Parallel
                       ann
                       videoParts
                       (spliceLeftMostOrRightMost position clips audioParts))
            , mapCompositionPart = const pure
            }
      in mapAtFocus focus traversal parent
    _ -> mzero
  where
    traverseWithParent t = withParentOf t focus parent
    spliceLeftMostOrRightMost pos new xs =
      case pos of
        LeftOf    -> spliceAt 0 new xs
        LeftMost  -> spliceAt 0 new xs
        RightOf   -> xs <> new
        RightMost -> xs <> new

-- | Same as 'insert', but returns the original 'Composition' if the
-- insertion failed.
insert_
  :: Focus ft
  -> Insertion a
  -> InsertPosition
  -> Composition TimelineType a
  -> Composition TimelineType a
insert_ f i p s = fromMaybe s (insert f i p s)
