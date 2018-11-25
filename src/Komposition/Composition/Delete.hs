{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE GADTs         #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ViewPatterns  #-}
-- | Transform a composition by deleting children.

module Komposition.Composition.Delete where

import           Komposition.Prelude

import           Control.Lens
import qualified Data.List                     as List
import qualified Data.List.NonEmpty            as NonEmpty

import           Komposition.Composition
import           Komposition.Focus
import           Komposition.Focus.Parent

-- | Delete a composition or part in the 'Timeline', at the 'Focus',
-- returning a new 'Timeline' if the focus is valid, and possibly a
-- 'FocusCommand' required to obtain a new valid focus into the new
-- 'Timeline'.
delete :: Focus (ToFocusType Timeline) -> Timeline a -> Maybe (DeletionResult a)
delete focus comp = do
  (tl, st ) <- runStateT (withParentOf traversal focus comp) Nothing
  (sc, cmd) <- st
  pure (DeletionResult tl sc cmd)
  where
    traversal = ParentTraversal
      { onTimeline   = \i (Timeline children') -> do
        (deleted, remaining) <- lift (deleteAt NonEmpty.splitAt i children')
        put (Just (SomeSequence deleted, commandIfAtEnd children' i))
        maybe mzero (pure . Timeline) (NonEmpty.nonEmpty remaining)
      , onSequence   = \i (Sequence ann children') -> do
        (deleted, remaining) <- lift (deleteAt NonEmpty.splitAt i children')
        put (Just (SomeParallel deleted, commandIfAtEnd children' i))
        maybe mzero (pure . Sequence ann) (NonEmpty.nonEmpty remaining)
      , onVideoParts = \i vs -> do
        (deleted, remaining) <- lift (deleteAt List.splitAt i vs)
        put (Just (SomeVideoPart deleted, commandIfAtEnd vs i))
        pure remaining
      , onAudioParts = \i as -> do
        (deleted, remaining) <- lift (deleteAt List.splitAt i as)
        put (Just (SomeAudioPart deleted, commandIfAtEnd as i))
        pure remaining
      }
    commandIfAtEnd :: Foldable t => t a -> Int -> Maybe FocusCommand
    commandIfAtEnd (length -> 1) _ = pure FocusUp
    commandIfAtEnd (pred . length -> maxIndex) idx
      | idx >= maxIndex = pure FocusLeft
      | otherwise       = mzero

-- | Same as 'delete', but trying to apply the returned focus command.
delete_
  :: ft ~ ToFocusType Timeline
  => Focus ft
  -> Timeline a
  -> Either (FocusCommand, FocusError) (Timeline a, Focus ft)
delete_ f s = case delete f s of
  Nothing -> pure (s, f)
  Just r  -> case adjustingFocusCommand r of
    Nothing -> pure (resultingTimeline r, f)
    Just cmd ->
      modifyFocus s cmd f & _Left %~ (cmd, ) <&> (resultingTimeline r, )

-- | In case a deletion was successful, this data type describes what
-- the resulting timeline is, what in the composition was deleted, and
-- a possible focus command that needs to be applied in order to keep
-- the focus consistent with regards to the new timeline.
data DeletionResult a =
  DeletionResult
  { resultingTimeline :: Timeline a
  , deletedComposition :: SomeComposition a
  , adjustingFocusCommand :: Maybe FocusCommand
  }

deleteAt :: (Int -> t a -> ([a], [a])) -> Int -> t a -> Maybe (a, [a])
deleteAt splitAt' i xs = case splitAt' i xs of
  (before, a : as) -> pure (a, before <> as)
  (_     , []    ) -> mzero
