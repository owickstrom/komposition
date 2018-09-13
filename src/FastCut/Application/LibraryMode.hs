{-# OPTIONS_GHC -fno-warn-unticked-promoted-constructors #-}

{-# LANGUAGE ConstraintKinds  #-}
{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE GADTs            #-}
{-# LANGUAGE LambdaCase       #-}
{-# LANGUAGE RankNTypes       #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE TypeOperators    #-}
module FastCut.Application.LibraryMode where

import           FastCut.Application.Base

import           Control.Lens
import qualified Data.List.NonEmpty          as NonEmpty
import           Data.Row.Records

import           FastCut.Composition
import           FastCut.Composition.Insert
import           FastCut.Focus
import           FastCut.Library
import           FastCut.MediaType
import           FastCut.Project

import           FastCut.Application.KeyMaps

selectAssetFromList
  :: (UserInterface m, IxMonadIO m, Modify n (State m LibraryMode) r ~ r)
  => Name n
  -> SelectAssetsModel mt
  -> Actions
       m
       '[n := Remain (State m LibraryMode)]
       r
       (Maybe [Asset mt])
selectAssetFromList gui model = do
  updateLibrary gui model
  nextEvent gui >>>= \case
    (LibraryAssetsSelected selectedMediaType newSelectedAssets) ->
      -- TODO: Can "LibraryMode" be parameterized on its media type to
      -- avoid this?
      case (mediaType model, selectedMediaType) of
        (SVideo, SVideo) ->
          continueWith model {selectedAssets = newSelectedAssets}
        (SAudio, SAudio) ->
          continueWith model {selectedAssets = newSelectedAssets}
        _ -> continueWith model
    LibrarySelectionConfirmed -> ireturn (Just (selectedAssets model))
    CommandKeyMappedEvent Cancel -> ireturn Nothing
    CommandKeyMappedEvent Help ->
      help gui [ModeKeyMap SLibraryMode (keymaps SLibraryMode)] >>>
      continueWith model
  where
    continueWith = selectAssetFromList gui

selectAsset ::
     (Application t m, r ~ (n .== State (t m) 'TimelineMode))
  => Name n
  -> Project
  -> Focus SequenceFocusType
  -> SMediaType mt
  -> t m r r (Maybe [Asset mt])
selectAsset gui project focus' = \case
  SVideo ->
    case NonEmpty.nonEmpty (project ^. library . videoAssets) of
      Just vs -> do
        let model = SelectAssetsModel SVideo vs []
        enterLibrary gui model
        assets <- selectAssetFromList gui model
        returnToTimeline gui project focus'
        ireturn assets
      Nothing -> ireturn Nothing
  SAudio ->
    case NonEmpty.nonEmpty (project ^. library . audioAssets) of
      Just as -> do
        let model = SelectAssetsModel SAudio as []
        enterLibrary gui model
        assets <- selectAssetFromList gui model
        returnToTimeline gui project focus'
        ireturn assets
      Nothing -> ireturn Nothing

selectAssetAndInsert ::
     (Application t m, r ~ (n .== State (t m) 'TimelineMode))
  => Name n
  -> Project
  -> Focus SequenceFocusType
  -> SMediaType mt
  -> InsertPosition
  -> t m r r Project
selectAssetAndInsert gui project focus' mediaType' position =
  selectAsset gui project focus' mediaType' >>= \case
    Just assets ->
      project
        &  timeline
        %~ insert_ focus' (insertionOf assets) position
        &  ireturn
    Nothing -> beep gui >>> ireturn project
 where
  insertionOf a = case mediaType' of
    SVideo -> InsertVideoParts (VideoClip () <$> a)
    SAudio -> InsertAudioParts (AudioClip () <$> a)
