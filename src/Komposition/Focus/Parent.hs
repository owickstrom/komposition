{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE GADTs           #-}
{-# LANGUAGE KindSignatures  #-}
{-# LANGUAGE RecordWildCards #-}
-- | Traverse a timeline to the parent of the focused composition,
-- clip or gap.

module Komposition.Focus.Parent where

import           Komposition.Prelude

import           Control.Lens            hiding (below)

import           Komposition.Composition
import           Komposition.Focus
import           Komposition.MediaType

data ParentAtFocus a where
  TimelineParent :: Timeline a -> ParentAtFocus a
  SequenceParent :: Sequence a -> ParentAtFocus a
  ParallelParent :: Parallel a -> ParentAtFocus a
  VideoTrackParent :: VideoTrack a -> ParentAtFocus a
  AudioTrackParent :: AudioTrack a -> ParentAtFocus a

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
  parentAtFocus (TrackFocus _ Nothing) p  = pure (ParallelParent p)
  parentAtFocus (TrackFocus mt (Just subFocus)) (Parallel _ videoTrack' audioTrack') =
    case mt of
      Video -> parentAtFocus subFocus videoTrack'
      Audio -> parentAtFocus subFocus audioTrack'

instance HasParentAtFocus VideoTrack where
  parentAtFocus _ t  = pure (VideoTrackParent t)

instance HasParentAtFocus AudioTrack where
  parentAtFocus _ t  = pure (AudioTrackParent t)


data ParentTraversal (m :: * -> *) a = ParentTraversal
  { onTimeline   :: Int -> Timeline a -> m (Timeline a)
  , onSequence   :: Int -> Sequence a -> m (Sequence a)
  , onParallel   :: MediaType -> Parallel a -> m (Parallel a)
  , onVideoTrack :: Int -> VideoTrack a -> m (VideoTrack a)
  , onAudioTrack :: Int -> AudioTrack a -> m (AudioTrack a)
  }

parentTraversal :: Applicative f => ParentTraversal f a
parentTraversal = ParentTraversal unchanged
                                  unchanged
                                  unchanged
                                  unchanged
                                  unchanged
  where unchanged = const pure

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
  withParentOf ParentTraversal {..} (TrackFocus clipType Nothing) p =
    onParallel clipType p
  withParentOf t@ParentTraversal {..} (TrackFocus clipType (Just clipFocus)) (Parallel ann videoTrack' audioTrack') =
        case clipType of
          Video -> videoTrack' & withParentOf t clipFocus & fmap (\track -> Parallel ann track audioTrack')
          Audio -> audioTrack' & withParentOf t clipFocus & fmap (Parallel ann videoTrack')

instance WithParentOf VideoTrack where
  withParentOf ParentTraversal {..} (ClipFocus idx) = onVideoTrack idx

instance WithParentOf AudioTrack where
  withParentOf ParentTraversal {..} (ClipFocus idx) = onAudioTrack idx
