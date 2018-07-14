{-# OPTIONS_GHC -fno-warn-unticked-promoted-constructors #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs     #-}
-- | Transform a 'Composition' by inserting children.

module FastCut.Composition.Insert where

import           FastCut.Prelude

import           FastCut.Composition
import           FastCut.Focus
import           FastCut.Focus.Parent
import           FastCut.MediaType

insertAt :: Int -> a -> [a] -> [a]
insertAt i x xs =
  let (before, after) = splitAt i xs
  in before <> (x : after)

data InsertPosition
  = LeftMost
  | LeftOf
  | RightOf
  | RightMost

data Insertion a
  = InsertSequence (Composition a SequenceType)
  | InsertParallel (Composition a ParallelType)
  | InsertVideoPart (CompositionPart a Video)
  | InsertAudioPart (CompositionPart a Audio)

-- | Inserts a 'Composition' or 'CompositionPart', wrapped in the
-- 'Insertion', relative to 'InsertPosition' and the 'Focus'.
insert ::
    Focus ft
  -> Insertion a
  -> InsertPosition
  -> Composition a TimelineType
  -> Maybe (Composition a TimelineType)
insert focus insertion position parent =
  traversal >>= \t -> withParentOfM t focus parent
  where
    traversal =
      case (insertion, position) of
        (InsertSequence sequence', RightOf) ->
          pure
            (parentTraversal
             { onTimeline =
                 \i (Timeline ann children') ->
                   pure (Timeline ann (insertAt (succ i) sequence' children'))
             })
        (InsertParallel parallel, RightOf) ->
          pure
            (parentTraversal
             { onSequence =
                 \i (Sequence ann children') ->
                   pure (Sequence ann (insertAt (succ i) parallel children'))
             })
        (InsertVideoPart part, RightOf) ->
          pure
            (parentTraversal
             {onVideoParts = \i -> pure . insertAt (succ i) part})
        (InsertAudioPart part, RightOf) ->
          pure
            (parentTraversal
             {onAudioParts = \i -> pure . insertAt (succ i) part})
        (_, _) -> mzero

-- | Same as 'insert', but returns the original 'Composition' if the
-- insertion failed.
insert_ ::
    Focus ft
  -> Insertion a
  -> InsertPosition
  -> Composition a TimelineType
  -> Composition a TimelineType
insert_ f i p s = fromMaybe s (insert f i p s)
