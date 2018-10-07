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

import           Control.Lens
import qualified Data.List.NonEmpty          as NonEmpty
import           Data.Row.Records
import           Data.String                 (fromString)

import           Komposition.Composition
import           Komposition.Composition.Insert
import           Komposition.Duration
import           Komposition.History
import           Komposition.Library
import           Komposition.MediaType
import           Komposition.Project
import qualified Komposition.Render.Composition  as Composition
import           Komposition.Render.FFmpeg

import           Komposition.Application.KeyMaps

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
  -> TimelineModel
  -> SMediaType mt
  -> t m r r (Maybe [Asset mt])
selectAsset gui model = \case
  SVideo ->
    case NonEmpty.nonEmpty (currentProject model ^. library . videoAssets) of
      Just vs -> do
        let libraryModel = SelectAssetsModel SVideo vs []
        enterLibrary gui libraryModel
        assets <- selectAssetFromList gui libraryModel
        returnToTimeline gui model
        ireturn assets
      Nothing -> ireturn Nothing
  SAudio ->
    case NonEmpty.nonEmpty (currentProject model ^. library . audioAssets) of
      Just as -> do
        let libraryModel = SelectAssetsModel SAudio as []
        enterLibrary gui libraryModel
        assets <- selectAssetFromList gui libraryModel
        returnToTimeline gui model
        ireturn assets
      Nothing -> ireturn Nothing

selectAssetAndInsert ::
     (Application t m, r ~ (n .== State (t m) 'TimelineMode))
  => Name n
  -> TimelineModel
  -> SMediaType mt
  -> InsertPosition
  -> t m r r TimelineModel
selectAssetAndInsert gui model mediaType' position =
  selectAsset gui model mediaType' >>= \case
    Just assets -> do
      i <- insertionOf assets
      model
        & existingProject . projectHistory %~ edit (\p -> p & timeline %~ insert_ (model ^. currentFocus) i position)
        & ireturn
    Nothing -> do
      beep gui
      ireturn (model & statusMessage .~ Just (noAssetsMessage mediaType'))
  where
    insertionOf a =
      case mediaType' of
        SVideo -> iliftIO (InsertVideoParts <$> mapM toVideoClip a)
        SAudio -> ireturn (InsertAudioParts (AudioClip () <$> a))
    toVideoClip :: VideoAsset -> IO (VideoPart ())
    toVideoClip videoAsset =
      let ts =
            maybe
              (TimeSpan 0 (durationOf videoAsset))
              snd
              (videoAsset ^. videoClassifiedScene)
      in VideoClip () videoAsset ts <$>
         extractFrameToFile
           (currentProject model ^. videoSettings)
           Composition.FirstFrame
           VideoProxy
           videoAsset
           ts
           (model ^. existingProject . projectPath . unProjectPath)

noAssetsMessage :: SMediaType mt -> Text
noAssetsMessage mt =
  "You have no " <> mt' <> " assets in your library. Use 'Import' to add some assets."
  where
    mt' = case mt of
      SVideo -> "video"
      SAudio -> "audio"
