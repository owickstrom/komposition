{-# OPTIONS_GHC -fno-warn-unticked-promoted-constructors #-}
{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE GADTs         #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ViewPatterns  #-}
-- | Transform a 'Composition' by deleting children.

module FastCut.Composition.Delete where

import           FastCut.Prelude

import           Control.Lens
import qualified Data.List            as List
import qualified Data.List.NonEmpty   as NonEmpty

import           FastCut.Composition
import           FastCut.Focus
import           FastCut.Focus.Parent

deleteAt :: (Int -> t a -> ([a], [a])) -> Int -> t a -> [a]
deleteAt splitAt' i xs =
  let (before, after) = splitAt' i xs
  in before <> drop 1 after

-- | Delete a 'Composition' or 'CompositionPart' at the 'Focus',
-- returning a new 'Composition' if the focus is valid, and possibly a
-- 'FocuCommand' required to obtain a new valid focus into the new
-- 'Composition'.
delete ::
    Focus ft
  -> Composition TimelineType a
  -> Maybe (Composition TimelineType a, Maybe FocusCommand)
delete focus comp = runStateT (withParentOf traversal focus comp) Nothing
  where
    traversal =
      ParentTraversal
      { onTimeline =
          \i (Timeline children') -> do
            moveIfAtEnd children' i
            maybe mzero (pure . Timeline) $
              NonEmpty.nonEmpty (deleteAt NonEmpty.splitAt i children')
      , onSequence =
        \i (Sequence ann children') -> do
            moveIfAtEnd children' i
            maybe mzero (pure . Sequence ann) $
              NonEmpty.nonEmpty (deleteAt NonEmpty.splitAt i children')
      , onVideoParts = \i vs -> moveIfAtEnd vs i *> pure (deleteAt List.splitAt i vs)
      , onAudioParts = \i as -> moveIfAtEnd as i *> pure (deleteAt List.splitAt i as)
      }
    moveIfAtEnd :: Foldable t => t a -> Int -> StateT (Maybe FocusCommand) Maybe ()
    moveIfAtEnd (length -> 1) _ = put (Just FocusUp)
    moveIfAtEnd (pred . length -> maxIndex) idx
      | idx >= maxIndex = put (Just FocusLeft)
      | otherwise = pure ()

-- | Same as 'delete', but trying to apply the returned focus command.
delete_ ::
    Focus ft
  -> Composition TimelineType a
  -> Either (FocusCommand, FocusError) (Composition TimelineType a, Focus ft)
delete_ f s =
  case delete f s of
    Nothing -> pure (s, f)
    Just (s', Nothing) -> pure (s', f)
    Just (s', Just cmd) ->
      modifyFocus s cmd f
      & _Left %~ (cmd,)
      <&> (s',)
