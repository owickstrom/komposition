{-# OPTIONS_GHC -fno-warn-unticked-promoted-constructors #-}
{-# LANGUAGE DataKinds  #-}
{-# LANGUAGE GADTs      #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
-- | Transform a 'Composition' by inserting children.

module FastCut.Composition.Insert where

import           FastCut.Prelude

import qualified Data.List.NonEmpty   as NonEmpty

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

data InsertPosition
  = LeftMost
  | LeftOf
  | RightOf
  | RightMost
  | IntoLeftMost
  | IntoRightMost

data Insertion a
  = InsertSequence (Composition a SequenceType)
  | InsertParallel (Composition a ParallelType)
  | InsertVideoPart (CompositionPart a Video)
  | InsertAudioPart (CompositionPart a Audio)

-- | Inserts a 'Composition' or 'CompositionPart', wrapped in the
-- 'Insertion', relative to 'InsertPosition' and the 'Focus'.
insert
  :: Focus ft
  -> Insertion a
  -> InsertPosition
  -> Composition a TimelineType
  -> Maybe (Composition a TimelineType)
insert focus insertion position parent =
  case (insertion, position) of
    (InsertSequence sequence', RightOf) ->
      traverseWithParent
        (parentTraversal
         { onTimeline =
             \i (Timeline ann children') ->
               pure
                 (Timeline ann (insertAtNonEmpty (succ i) sequence' children'))
         })
    (InsertParallel parallel, RightOf) ->
      traverseWithParent
        (parentTraversal
         { onSequence =
             \i (Sequence ann children') ->
               pure
                 (Sequence ann (insertAtNonEmpty (succ i) parallel children'))
         })
    (InsertVideoPart part, RightOf) ->
      traverseWithParent
        (parentTraversal {onVideoParts = \i -> pure . insertAt (succ i) part})
    (InsertAudioPart part, RightOf) ->
      traverseWithParent
        (parentTraversal {onAudioParts = \i -> pure . insertAt (succ i) part})
    (InsertVideoPart part, IntoLeftMost) ->
      let traversal = FocusedTraversal { mapSequence = pure
                                       , mapParallel = \(Parallel ann videoParts audioParts) ->
                                           pure (Parallel ann (insertAt 0 part videoParts) audioParts)
                                       , mapCompositionPart = const pure
                                       }
      in mapAtFocus focus traversal parent
    (_, _) -> mzero
  where
    traverseWithParent t = withParentOf t focus parent

-- | Same as 'insert', but returns the original 'Composition' if the
-- insertion failed.
insert_
  :: Focus ft
  -> Insertion a
  -> InsertPosition
  -> Composition a TimelineType
  -> Composition a TimelineType
insert_ f i p s = fromMaybe s (insert f i p s)
