{-# OPTIONS_GHC -fno-warn-unticked-promoted-constructors #-}

{-# LANGUAGE ConstraintKinds   #-}
{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE RebindableSyntax  #-}
{-# LANGUAGE TypeOperators     #-}
module FastCut.Application.LibraryMode where

import           FastCut.Application.Base

import           Control.Lens
import           Data.Row.Records

import           FastCut.Focus
import           FastCut.Composition
import           FastCut.Composition.Insert
import           FastCut.Library
import           FastCut.MediaType
import           FastCut.Project

import           FastCut.Application.KeyMaps

selectAssetFromList
  :: (UserInterface m, IxMonadIO m, Modify n (State m LibraryMode) r ~ r)
  => Name n
  -> [Asset mt]
  -> Int
  -> Actions
       m
       '[n := Remain (State m LibraryMode)]
       r
       (Maybe (Asset mt))
selectAssetFromList gui assets n = do
  updateLibrary gui assets n
  nextEvent gui >>>= \case
    CommandKeyMappedEvent Cancel -> ireturn Nothing
    CommandKeyMappedEvent Help ->
      help gui [ModeKeyMap SLibraryMode (keymaps SLibraryMode)] >>> continue
    CommandKeyMappedEvent LibrarySelect -> ireturn (assets ^? element n)
    CommandKeyMappedEvent LibraryUp
      | n > 0     -> selectAssetFromList gui assets (pred n)
      | otherwise -> continue
    CommandKeyMappedEvent LibraryDown
      | n < length assets - 1 -> selectAssetFromList gui assets (succ n)
      | otherwise             -> continue
  where continue = selectAssetFromList gui assets n

selectAsset
  :: Application t m
  => Name n
  -> Project
  -> Focus ft
  -> SMediaType mt
  -> ThroughMode
       TimelineMode
       LibraryMode
       (t m)
       n
       (Maybe (Asset mt))
selectAsset gui project focus' mediaType = case mediaType of
  SVideo -> do
    enterLibrary gui (project ^. library . videoAssets) 0
    asset' <- selectAssetFromList gui (project ^. library . videoAssets) 0
    returnToTimeline gui project focus'
    ireturn asset'
  SAudio -> do
    enterLibrary gui (project ^. library . audioAssets) 0
    asset' <- selectAssetFromList gui (project ^. library . audioAssets) 0
    returnToTimeline gui project focus'
    ireturn asset'

selectAssetAndInsert
  :: Application t m
  => Name n
  -> Project
  -> Focus ft
  -> SMediaType mt
  -> InsertPosition
  -> ThroughMode TimelineMode LibraryMode (t m) n Project
selectAssetAndInsert gui project focus' mediaType position =
  selectAsset gui project focus' mediaType >>= \case
    Just asset' ->
      project
        &  timeline
        %~ insert_ focus' (insertionOf asset') position
        &  ireturn
    Nothing -> ireturn project
 where
  insertionOf a = case mediaType of
    SVideo -> InsertVideoPart (Clip () a)
    SAudio -> InsertAudioPart (Clip () a)
