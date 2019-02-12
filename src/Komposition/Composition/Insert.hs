{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GADTs         #-}
{-# LANGUAGE LambdaCase    #-}
{-# LANGUAGE RankNTypes    #-}

-- | Transform a 'Composition' by inserting children.
module Komposition.Composition.Insert where

import           Komposition.Prelude

import           Control.Lens
import qualified Data.List.NonEmpty       as NonEmpty

import           Komposition.Composition
import           Komposition.Focus
import           Komposition.Focus.Parent
import           Komposition.MediaType

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

indexForInsertPosition :: InsertPosition -> Int -> Int -> Int
indexForInsertPosition  LeftMost  _ _     = 0
indexForInsertPosition  LeftOf    _ curr  = curr
indexForInsertPosition  RightOf   _ curr  = curr + 1
indexForInsertPosition  RightMost max'  _ = max'

data InsertPosition
  = LeftMost
  | LeftOf
  | RightOf
  | RightMost
  deriving (Show, Eq, Ord, Enum, Bounded, Generic)

data Insertion a
  = InsertSequence (Sequence a)
  | InsertParallel (Parallel a)
  | InsertVideoParts (NonEmpty (TrackPart 'Video a))
  | InsertAudioParts (NonEmpty (TrackPart 'Audio a))
  deriving (Show, Eq, Generic)

-- | Inserts a 'Composition' or 'TrackPart', wrapped in the
-- 'Insertion', relative to 'InsertPosition' and the 'Focus'.
insert
  :: Focus (ToFocusType Timeline)
  -> Insertion a
  -> InsertPosition
  -> Timeline a
  -> Maybe (InsertionResult a)
insert oldFocus insertion position parent = do
  (tl, mNewFocus) <- runStateT go Nothing
  InsertionResult tl <$> mNewFocus
  where
    go =
      case (insertion, oldFocus) of
        (InsertSequence sequence', SequenceFocus _ Nothing) ->
          traverseWithParent
            (parentTraversal
             { onTimeline = \i (Timeline children') -> do
                 oldFocus
                   & leafFocusIndex .~ indexForInsertPosition position (length children') i
                   & pure
                   & put
                 Timeline <$> insertAtPositionInNonEmpty position i children' sequence'
             })
        (InsertParallel parallel, SequenceFocus _ (Just (ParallelFocus _ Nothing))) ->
          traverseWithParent
            (parentTraversal
             { onSequence = \i (Sequence ann children') -> do
                 oldFocus
                   & leafFocusIndex %~ indexForInsertPosition position (length children')
                   & pure
                   & put
                 Sequence ann <$> insertAtPositionInNonEmpty position i children' parallel
             })
        (InsertVideoParts clips, SequenceFocus sIdx (Just (ParallelFocus pIdx Nothing))) ->
          let traversal =
                FocusedTraversal
                { mapSequence = pure
                , mapParallel = \(Parallel ann (VideoTrack vAnn videoParts) audioTrack) -> do
                    let (newParts, newClipIdx) = spliceLeftMostOrRightMost position (NonEmpty.toList clips) videoParts
                    put . pure $
                      SequenceFocus sIdx (Just (ParallelFocus pIdx (Just (TrackFocus Video (Just (ClipFocus newClipIdx))))))
                    pure (Parallel ann (VideoTrack vAnn newParts) audioTrack)
                , mapVideoTrack = pure
                , mapAudioTrack = pure
                , mapTrackPart = const pure
                }
          in mapAtFocus oldFocus traversal parent
        (InsertAudioParts clips, SequenceFocus sIdx (Just (ParallelFocus pIdx Nothing))) ->
          let traversal =
                FocusedTraversal
                { mapSequence = pure
                , mapParallel = \(Parallel ann videoTrack (AudioTrack aAnn audioParts)) -> do
                    let (newParts, newClipIdx) = spliceLeftMostOrRightMost position (NonEmpty.toList clips) audioParts
                    put . pure $
                      SequenceFocus sIdx (Just (ParallelFocus pIdx (Just (TrackFocus Audio (Just (ClipFocus newClipIdx))))))
                    pure (Parallel ann videoTrack (AudioTrack aAnn newParts))
                , mapVideoTrack = pure
                , mapAudioTrack = pure
                , mapTrackPart = const pure
                }
          in mapAtFocus oldFocus traversal parent
        (InsertVideoParts clips, SequenceFocus sIdx (Just (ParallelFocus pIdx (Just (TrackFocus Video Nothing))))) ->
          let traversal =
                FocusedTraversal
                { mapSequence = pure
                , mapParallel = pure
                , mapVideoTrack = \(VideoTrack vAnn videoParts) -> do
                    let (newParts, newClipIdx) = spliceLeftMostOrRightMost position (NonEmpty.toList clips) videoParts
                    put . pure $
                      SequenceFocus sIdx (Just (ParallelFocus pIdx (Just (TrackFocus Video (Just (ClipFocus newClipIdx))))))
                    pure (VideoTrack vAnn newParts)
                , mapAudioTrack = pure
                , mapTrackPart = const pure
                }
          in mapAtFocus oldFocus traversal parent
        (InsertAudioParts clips, SequenceFocus sIdx (Just (ParallelFocus pIdx (Just (TrackFocus Audio Nothing))))) ->
          let traversal =
                FocusedTraversal
                { mapSequence = pure
                , mapParallel = pure
                , mapVideoTrack = pure
                , mapAudioTrack = \(AudioTrack vAnn audioParts) -> do
                    let (newParts, newClipIdx) = spliceLeftMostOrRightMost position (NonEmpty.toList clips) audioParts
                    put . pure $
                      SequenceFocus sIdx (Just (ParallelFocus pIdx (Just (TrackFocus Audio (Just (ClipFocus newClipIdx))))))
                    pure (AudioTrack vAnn newParts)
                , mapTrackPart = const pure
                }
          in mapAtFocus oldFocus traversal parent
        (InsertVideoParts clips, SequenceFocus _ (Just (ParallelFocus _ (Just (TrackFocus Video (Just (ClipFocus _))))))) ->
          traverseWithParent (parentTraversal {onVideoTrack = \i (VideoTrack ann vs) -> do
                                                  oldFocus
                                                    & leafFocusIndex %~ indexForInsertPosition position (length vs)
                                                    & pure
                                                    & put
                                                  VideoTrack ann <$> spliceAtPositionInList position i (NonEmpty.toList clips) vs
                                              })
        (InsertAudioParts clips, SequenceFocus _ (Just (ParallelFocus _ (Just (TrackFocus Audio (Just (ClipFocus _))))))) ->
          traverseWithParent (parentTraversal {onAudioTrack = \i (AudioTrack ann as) -> do
                                                  oldFocus
                                                    & leafFocusIndex %~ indexForInsertPosition position (length as)
                                                    & pure
                                                    & put
                                                  AudioTrack ann <$> spliceAtPositionInList position i (NonEmpty.toList clips) as
                                              })
        _ -> mzero
    traverseWithParent t = withParentOf t oldFocus parent
    spliceLeftMostOrRightMost pos new xs =
      case pos of
        LeftOf    -> (spliceAt 0 new xs, 0)
        LeftMost  -> (spliceAt 0 new xs, 0)
        RightOf   -> (xs <> new, length xs)
        RightMost -> (xs <> new, length xs)

data InsertionResult a =
  InsertionResult
  { resultingTimeline :: Timeline a
  , resultingFocus    :: Focus 'SequenceFocusType
  }

insert_
  :: Focus 'SequenceFocusType
  -> Insertion a
  -> InsertPosition
  -> Timeline a
  -> Maybe (Timeline a, Focus 'SequenceFocusType)
insert_ oldFocus insertion pos tl = do
  r <- insert oldFocus insertion pos tl
  pure (resultingTimeline r, resultingFocus r)

insertionFromSomeComposition :: SomeComposition a -> Maybe (Insertion a)
insertionFromSomeComposition = \case
  SomeSequence   s                 -> pure (InsertSequence s)
  SomeParallel   p                 -> pure (InsertParallel p)
  SomeVideoTrack (VideoTrack _ vs) -> InsertVideoParts <$> NonEmpty.nonEmpty vs
  SomeAudioTrack (AudioTrack _ as) -> InsertAudioParts <$> NonEmpty.nonEmpty as
  SomeVideoPart  v                 -> pure (InsertVideoParts (pure v))
  SomeAudioPart  a                 -> pure (InsertAudioParts (pure a))
