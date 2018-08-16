{-# OPTIONS_GHC -fno-warn-unticked-promoted-constructors #-}
{-# LANGUAGE DataKinds  #-}
{-# LANGUAGE GADTs      #-}
{-# LANGUAGE RankNTypes #-}

-- | Transform a 'Composition' by inserting children.
module FastCut.Composition.Insert where

import           FastCut.Prelude

import qualified Data.List.NonEmpty            as NonEmpty

import           FastCut.Composition
import           FastCut.Focus
import           FastCut.Focus.Parent
import           FastCut.MediaType

insertAt :: Int -> a -> [a] -> [a]
insertAt i x xs = let (before, after) = splitAt i xs in before <> (x : after)

insertAtNonEmpty :: Int -> a -> NonEmpty a -> NonEmpty a
insertAtNonEmpty i x xs =
  let (before, after) = NonEmpty.splitAt i xs
  in  NonEmpty.fromList (before <> (x : after))

insertAtPositionInList :: Applicative f => InsertPosition -> Int -> [a] -> a -> f [a]
insertAtPositionInList position i xs x =
  case position of
    LeftMost -> pure (x : xs)
    LeftOf -> pure (insertAt i x xs)
    RightOf -> pure (insertAt (succ i) x xs)
    RightMost -> pure (xs <> [x])

insertAtPositionInNonEmpty :: Applicative f => InsertPosition -> Int -> NonEmpty a -> a -> f (NonEmpty a)
insertAtPositionInNonEmpty position i xs x =
  case position of
    LeftMost -> pure (pure x <> xs)
    LeftOf -> pure (insertAtNonEmpty i x xs)
    RightOf -> pure (insertAtNonEmpty (succ i) x xs)
    RightMost -> pure (xs <> pure x)


data InsertPosition
  = LeftMost
  | LeftOf
  | RightOf
  | RightMost
  deriving (Show, Eq, Enum, Bounded)

data Insertion a
  = InsertSequence (Composition a SequenceType)
  | InsertParallel (Composition a ParallelType)
  | InsertVideoPart (CompositionPart a Video)
  | InsertAudioPart (CompositionPart a Audio)
  deriving (Show, Eq)

-- | Inserts a 'Composition' or 'CompositionPart', wrapped in the
-- 'Insertion', relative to 'InsertPosition' and the 'Focus'.
insert
  :: Focus ft
  -> Insertion a
  -> InsertPosition
  -> Composition a TimelineType
  -> Maybe (Composition a TimelineType)
insert focus insertion position parent = case (insertion, position, focusType focus) of
  (InsertSequence sequence', _, SequenceFocusType) -> traverseWithParent
    (parentTraversal
      { onTimeline =
        \i (Timeline ann children') ->
          Timeline ann <$> insertAtPositionInNonEmpty position i children' sequence'
      }
    )
  (InsertParallel parallel, _, _) -> traverseWithParent
    (parentTraversal
      { onSequence =
        \i (Sequence ann children') ->
          Sequence ann <$> insertAtPositionInNonEmpty position i children' parallel
      }
    )
  (InsertVideoPart part, _, ClipFocusType) -> traverseWithParent
    (parentTraversal { onVideoParts = \i vs -> insertAtPositionInList position i vs part })
  (InsertAudioPart part, _, ClipFocusType) -> traverseWithParent
    (parentTraversal { onAudioParts = \i as -> insertAtPositionInList position i as part })
  (InsertVideoPart part, _, ParallelFocusType)
    -> let
         traversal = FocusedTraversal
           { mapSequence        = pure
           , mapParallel        = \(Parallel ann videoParts audioParts) ->
             case position of
               LeftOf ->
                 pure (Parallel ann (insertAt 0 part videoParts) audioParts)
               LeftMost ->
                 pure (Parallel ann (insertAt 0 part videoParts) audioParts)
               RightOf ->
                 pure (Parallel ann (videoParts <> [part]) audioParts)
               RightMost ->
                 pure (Parallel ann (videoParts <> [part]) audioParts)
           , mapCompositionPart = const pure
           }
       in  mapAtFocus focus traversal parent
  _ -> mzero
  where traverseWithParent t = withParentOf t focus parent

-- | Same as 'insert', but returns the original 'Composition' if the
-- insertion failed.
insert_
  :: Focus ft
  -> Insertion a
  -> InsertPosition
  -> Composition a TimelineType
  -> Composition a TimelineType
insert_ f i p s = fromMaybe s (insert f i p s)
