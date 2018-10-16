{-# LANGUAGE ConstraintKinds  #-}
{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs            #-}
{-# LANGUAGE LambdaCase       #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE RankNTypes       #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE TypeOperators    #-}
module Komposition.Application.LibraryMode where

import           Komposition.Application.Base

import           Data.Row.Records

import           Komposition.Library
import           Komposition.MediaType

import           Komposition.Application.KeyMaps

selectAssetFromList
  :: ( Application t m
     , r ~ ("library" .== Window (t m) (Event LibraryMode))
     )
  => SelectAssetsModel mt
  -> t m r r (Maybe [Asset mt])
selectAssetFromList model = do
  patchWindow #library (libraryView model)
  nextEvent #library >>>= \case
    (LibraryAssetsSelected selectedMediaType newSelectedAssets) ->
      -- TODO: Can "LibraryMode" be parameterized on its media type to
      -- avoid this?
      case (mediaType model, selectedMediaType) of
        (SVideo, SVideo) ->
          continueWith model { selectedAssets = newSelectedAssets }
        (SAudio, SAudio) ->
          continueWith model { selectedAssets = newSelectedAssets }
        _ -> continueWith model
    LibrarySelectionConfirmed    -> ireturn (Just (selectedAssets model))
    CommandKeyMappedEvent Cancel -> ireturn Nothing
    CommandKeyMappedEvent Help ->
      help [ModeKeyMap SLibraryMode (keymaps SLibraryMode)]
        >>> continueWith model
    WindowClosed -> ireturn Nothing
  where continueWith = selectAssetFromList

selectAsset
  :: ( Application t m
     )
  => SelectAssetsModel mt
  -> t m r r (Maybe [Asset mt])
selectAsset libraryModel =
  withNewWindow
    #library
    (libraryView libraryModel)
    (CommandKeyMappedEvent <$> keymaps SLibraryMode)
    (selectAssetFromList libraryModel)
