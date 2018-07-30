{-# LANGUAGE KindSignatures  #-}
{-# LANGUAGE TupleSections   #-}
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
import           FastCut.MediaType

type TraversalFunction m parent = Int -> parent -> m parent

data ParentTraversal (m :: * -> *) a = ParentTraversal
  { onTimeline   :: TraversalFunction m (Composition a TimelineType)
  , onSequence   :: TraversalFunction m (Composition a SequenceType)
  , onVideoParts :: TraversalFunction m [CompositionPart a Video]
  , onAudioParts :: TraversalFunction m [CompositionPart a Audio]
  }

parentTraversal :: Applicative f => ParentTraversal f a
parentTraversal = ParentTraversal unchanged unchanged unchanged unchanged
  where
    unchanged = const pure

withParentOf ::
  (MonadPlus m, Monad m)
  => ParentTraversal m a
  -> Focus ft
  -> Composition a t
  -> m (Composition a t)
withParentOf t@ParentTraversal {..} f s =
  case (f, s) of
    (SequenceFocus idx Nothing, Timeline ann sub) ->
      onTimeline idx (Timeline ann sub)
    (SequenceFocus idx (Just subFocus), Timeline ann sub) ->
      sub
        & ix idx %%~ withParentOf t subFocus
        & fmap (Timeline ann)
    (ParallelFocus i Nothing, Sequence ann sub) ->
      onSequence i (Sequence ann sub)
    (ParallelFocus idx (Just subFocus), Sequence ann sub) ->
      sub
        & ix idx %%~ withParentOf t subFocus
        & fmap (Sequence ann)
    (ClipFocus clipType i, Parallel ann videoParts audioParts) ->
      case clipType of
        Video ->
          onVideoParts i videoParts >>= \vs ->
            pure (Parallel ann vs audioParts)
        Audio ->
          onAudioParts i audioParts >>= \as ->
            pure (Parallel ann videoParts as)
    _ -> mzero
