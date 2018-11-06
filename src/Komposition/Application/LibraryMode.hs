{-# LANGUAGE ConstraintKinds   #-}
{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE RebindableSyntax  #-}
{-# LANGUAGE TypeOperators     #-}
module Komposition.Application.LibraryMode where

import           Komposition.Application.Base

import           Data.Row.Records

import           Komposition.Library
import           Komposition.MediaType

import           Komposition.Application.KeyMaps

selectAssetFromList
  :: (UserInterface m, Modify n (State m LibraryMode) r ~ r)
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
     ( Application t m sig
     )
  => Name n
  -> SelectAssetsModel mt
  -> ThroughMode TimelineMode LibraryMode (t m) n (Maybe [Asset mt])
selectAsset gui libraryModel returnToOrigin = do
  enterLibrary gui libraryModel
  selectAssetFromList gui libraryModel >>>= returnToOrigin
