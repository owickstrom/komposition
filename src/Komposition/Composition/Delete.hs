{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GADTs         #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ViewPatterns  #-}
-- | Transform a composition by deleting children.

module Komposition.Composition.Delete where

import           Komposition.Prelude

import           Control.Lens
import qualified Data.List.NonEmpty             as NonEmpty

import           Komposition.Composition
import           Komposition.Composition.Insert
import           Komposition.Focus
import           Komposition.Focus.Parent
import           Komposition.MediaType

-- | Specifies the number of video or audio parts to delete. Not used
-- for other composition parts.
newtype DeletionOf = DeletionOf Int
  deriving (Eq, Show, Generic)

-- | Delete a composition or part in the 'Timeline', at the 'Focus',
-- returning a new 'Timeline' if the focus is valid.
delete :: Focus (ToFocusType Timeline) -> DeletionOf -> Timeline a -> Maybe (DeletionResult a)
delete oldFocus deletionOf timeline' = do
  (tl, st ) <- runStateT (withParentOf traversal oldFocus timeline') Nothing
  (inverseInsertion', newFocus) <- st
  pure (DeletionResult tl inverseInsertion' newFocus)
  where
    traversal = ParentTraversal
      { onTimeline   = \i (Timeline children') -> do
        (deleted, remaining) <- lift (deleteAtNonEmpty i children')
        newFocus <- lift (changeFocusOnDelete oldFocus i children')
        put (Just ((InsertSequence deleted, insertPositionOnDelete i children'), newFocus))
        maybe mzero (pure . Timeline) (NonEmpty.nonEmpty remaining)
      , onSequence   = \i (Sequence ann children') -> do
        (deleted, remaining) <- lift (deleteAtNonEmpty i children')
        newFocus <- lift (changeFocusOnDelete oldFocus i children')
        put (Just ((InsertParallel deleted, insertPositionOnDelete i children'), newFocus))
        maybe mzero (pure . Sequence ann) (NonEmpty.nonEmpty remaining)
      , onParallel   = \mt (Parallel ann (VideoTrack vAnn vs) (AudioTrack aAnn as)) ->
          case mt of
            Video -> do
              deleted <- lift (NonEmpty.nonEmpty vs)
              put (Just ((InsertVideoParts deleted, LeftMost), oldFocus))
              pure (Parallel ann (VideoTrack vAnn mempty) (AudioTrack aAnn as))
            Audio -> do
              deleted <- lift (NonEmpty.nonEmpty as)
              put (Just ((InsertAudioParts deleted, LeftMost), oldFocus))
              pure (Parallel ann (VideoTrack vAnn vs) (AudioTrack aAnn mempty))
      , onVideoTrack = \i (VideoTrack ann vs) -> do
        (deleted, remaining) <- lift (deleteManyAt i deletionOf vs)
        newFocus <- lift (changeFocusOnDelete oldFocus i vs)
        put (Just ((InsertVideoParts deleted, insertPositionOnDelete i vs), newFocus))
        pure (VideoTrack ann remaining)
      , onAudioTrack = \i (AudioTrack ann as) -> do
        (deleted, remaining) <- lift (deleteManyAt i deletionOf as)
        newFocus <- lift (changeFocusOnDelete oldFocus i as)
        put (Just ((InsertAudioParts deleted, insertPositionOnDelete i as), newFocus))
        pure (AudioTrack ann remaining)
      }

onlyOne :: [a] -> Maybe a
onlyOne [a] = Just a
onlyOne _   = Nothing

changeFocusOnDelete :: Foldable t => Focus ft -> Int -> t a -> Maybe (Focus ft)
changeFocusOnDelete oldFocus i (length -> len)
  | len == 1 = changeFocusUp oldFocus
  | succ i == len = oldFocus & leafFocusIndex %~ pred & pure
  | otherwise = pure oldFocus

insertPositionOnDelete :: Foldable t => Int -> t a -> InsertPosition
insertPositionOnDelete i xs
  | i == pred (length xs) = RightOf
  | otherwise = LeftOf

data DeletionResult a =
  DeletionResult
  { resultingTimeline :: Timeline a
  , inverseInsertion  :: (Insertion a, InsertPosition)
  , resultingFocus    :: Focus 'SequenceFocusType
  }
  deriving (Eq, Show)

deleteAtNonEmpty :: Int -> NonEmpty a -> Maybe (a, [a])
deleteAtNonEmpty i xs = case NonEmpty.splitAt i xs of
  (_, [])        -> mzero
  (before, a:as) -> pure (a, before <> as)

deleteManyAt :: Int -> DeletionOf -> [a] -> Maybe (NonEmpty a, [a])
deleteManyAt i (DeletionOf n) xs = case splitAt i xs of
  (before, after)
    | null after -> mzero
    | otherwise ->
      let (deleted, after') = splitAt n after
      in (, before <> after') <$> NonEmpty.nonEmpty deleted
