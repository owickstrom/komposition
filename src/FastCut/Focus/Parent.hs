{-# LANGUAGE KindSignatures  #-}
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
  TimelineParent :: Timeline a -> ParentAtFocus a
  SequenceParent :: Sequence a -> ParentAtFocus a
  ParallelParent :: Parallel a -> ParentAtFocus a

class HasParentAtFocus t where
  parentAtFocus :: Focus (ToFocusType t) -> t a -> Maybe (ParentAtFocus a)

instance HasParentAtFocus Timeline where
  parentAtFocus (SequenceFocus _ Nothing) (Timeline sub) =
    pure (TimelineParent (Timeline sub))
  parentAtFocus (SequenceFocus idx (Just subFocus)) (Timeline sub) =
    parentAtFocus subFocus =<< toList sub `atMay` idx

instance HasParentAtFocus Sequence where
  parentAtFocus (ParallelFocus _ Nothing) (Sequence ann sub) =
    pure (SequenceParent (Sequence ann sub))
  parentAtFocus (ParallelFocus idx (Just subFocus)) (Sequence _ sub) =
    parentAtFocus subFocus =<< toList sub `atMay` idx

instance HasParentAtFocus Parallel where
  parentAtFocus (ClipFocus {}) (Parallel ann videoParts audioParts) =
    pure (ParallelParent (Parallel ann videoParts audioParts))


type TraversalFunction m parent = Int -> parent -> m parent

data ParentTraversal (m :: * -> *) a = ParentTraversal
  { onTimeline   :: TraversalFunction m (Timeline a)
  , onSequence   :: TraversalFunction m (Sequence a)
  , onVideoParts :: TraversalFunction m [CompositionPart Video a]
  , onAudioParts :: TraversalFunction m [CompositionPart Audio a]
  }

parentTraversal :: Applicative f => ParentTraversal f a
parentTraversal = ParentTraversal unchanged unchanged unchanged unchanged
  where
    unchanged = const pure

class WithParentOf t where
  withParentOf ::
       (MonadPlus m, Monad m)
    => ParentTraversal m a
    -> Focus (ToFocusType t)
    -> t a
    -> m (t a)

instance WithParentOf Timeline where
  withParentOf t@ParentTraversal {..} f s =
    case (f, s) of
      (SequenceFocus idx Nothing, Timeline sub) -> onTimeline idx (Timeline sub)
      (SequenceFocus idx (Just subFocus), Timeline sub) ->
        sub & ix idx %%~ withParentOf t subFocus & fmap Timeline

instance WithParentOf Sequence where
  withParentOf t@ParentTraversal {..} f s =
    case (f, s) of
      (ParallelFocus i Nothing, Sequence ann sub) ->
        onSequence i (Sequence ann sub)
      (ParallelFocus idx (Just subFocus), Sequence ann sub) ->
        sub & ix idx %%~ withParentOf t subFocus & fmap (Sequence ann)

instance WithParentOf Parallel where
  withParentOf ParentTraversal {..} (ClipFocus clipType i) (Parallel ann videoParts audioParts) =
        case clipType of
          Video ->
            onVideoParts i videoParts >>= \vs ->
              pure (Parallel ann vs audioParts)
          Audio ->
            onAudioParts i audioParts >>= \as ->
              pure (Parallel ann videoParts as)
