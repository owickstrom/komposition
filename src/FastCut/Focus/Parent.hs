{-# OPTIONS_GHC -fno-warn-unticked-promoted-constructors #-}
{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE GADTs           #-}
{-# LANGUAGE RecordWildCards #-}
-- | Traverse a timeline to the parent of the focused composition,
-- clip or gap.

module FastCut.Focus.Parent where

import           FastCut.Prelude

import           Control.Lens        hiding (below)
import           FastCut.Composition
import           FastCut.Focus

data ParentTraversal m a = ParentTraversal
  { onTimeline :: Int -> Composition a TimelineType  -> m (Composition a TimelineType)
  , onSequence   :: Int -> Composition a SequenceType -> m (Composition a SequenceType)
  , onVideoParts :: Int -> [CompositionPart a Video] -> m [CompositionPart a Video]
  , onAudioParts :: Int -> [CompositionPart a Audio] -> m [CompositionPart a Audio]
  }

parentTraversal :: Applicative m => ParentTraversal m a
parentTraversal =
  ParentTraversal
    (const pure)
    (const pure)
    (const pure)
    (const pure)

withParentOfM ::
  (MonadPlus m, Monad m)
  => ParentTraversal m a
  -> Focus ft
  -> Composition a t
  -> m (Composition a t)
withParentOfM t@ParentTraversal{..} f s =
  case (f, s) of
    (SequenceFocus idx Nothing, Timeline ann sub) ->
      onTimeline idx (Timeline ann sub)
    (SequenceFocus idx (Just subFocus), Timeline ann sub) ->
      sub
      & ix idx %%~ withParentOfM t subFocus
      & fmap (Timeline ann)
    (ParallelFocus idx Nothing, Sequence ann sub) ->
      onSequence idx (Sequence ann sub)
    (ParallelFocus idx (Just subFocus), Sequence ann sub) ->
      sub
      & ix idx %%~ withParentOfM t subFocus
      & fmap (Sequence ann)
    (ClipFocus clipType idx, Parallel ann videoParts audioParts) ->
      case clipType of
        Video -> onVideoParts idx videoParts >>= \vs ->
          pure (Parallel ann vs audioParts)
        Audio -> onAudioParts idx audioParts >>= \as ->
          pure (Parallel ann videoParts as)
    _ -> mzero
