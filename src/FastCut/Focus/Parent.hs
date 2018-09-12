{-# LANGUAGE KindSignatures  #-}
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

data ParentAtFocus a where
  TimelineParent :: Composition TimelineType a -> ParentAtFocus a
  SequenceParent :: Composition SequenceType a -> ParentAtFocus a
  ParallelParent :: Composition ParallelType a -> ParentAtFocus a

parentAtFocus ::
  Focus ft
  -> Composition t a
  -> Maybe (ParentAtFocus a)
parentAtFocus f s =
  case (f, s) of
    (SequenceFocus _ Nothing, Timeline sub) ->
      pure (TimelineParent (Timeline sub))
    (SequenceFocus idx (Just subFocus), Timeline sub) ->
      parentAtFocus subFocus =<< toList sub `atMay` idx
    (ParallelFocus _ Nothing, Sequence ann sub) ->
      pure (SequenceParent (Sequence ann sub))
    (ParallelFocus idx (Just subFocus), Sequence _ sub) ->
      parentAtFocus subFocus =<< toList sub `atMay` idx
    (ClipFocus{}, Parallel ann videoParts audioParts) ->
        pure (ParallelParent (Parallel ann videoParts audioParts))
    _ -> mzero


type TraversalFunction m parent = Int -> parent -> m parent

data ParentTraversal (m :: * -> *) a = ParentTraversal
  { onTimeline   :: TraversalFunction m (Composition TimelineType a)
  , onSequence   :: TraversalFunction m (Composition SequenceType a)
  , onVideoParts :: TraversalFunction m [CompositionPart Video a]
  , onAudioParts :: TraversalFunction m [CompositionPart Audio a]
  }

parentTraversal :: Applicative f => ParentTraversal f a
parentTraversal = ParentTraversal unchanged unchanged unchanged unchanged
  where
    unchanged = const pure

withParentOf ::
  (MonadPlus m, Monad m)
  => ParentTraversal m a
  -> Focus ft
  -> Composition t a
  -> m (Composition t a)
withParentOf t@ParentTraversal {..} f s =
  case (f, s) of
    (SequenceFocus idx Nothing, Timeline sub) ->
      onTimeline idx (Timeline sub)
    (SequenceFocus idx (Just subFocus), Timeline sub) ->
      sub
        & ix idx %%~ withParentOf t subFocus
        & fmap Timeline
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
