{-# LANGUAGE ConstraintKinds     #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedLabels    #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE PolyKinds           #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE RebindableSyntax    #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}
module Komposition.Application.ImportMode where

import           Komposition.Application.Base

import           Control.Lens
import           Data.String                 (fromString)

import           Komposition.Import.Audio
import           Komposition.Import.Video
import           Komposition.Library
import           Komposition.MediaType
import           Komposition.Project
import           Komposition.History

import           Komposition.Application.KeyMaps

data ImportFileForm = ImportFileForm
  { selectedFile :: Maybe FilePath
  , autoSplit    :: Bool
  }

importFile
  :: Application t m
  => Name n
  -> TimelineModel
  -> ThroughMode TimelineMode ImportMode (t m) n TimelineModel
importFile gui timelineModel = do
  let initialModel =
        ImportFileModel {autoSplitValue = False, autoSplitAvailable = True}
  enterImport gui initialModel
  f <- fillForm initialModel
                ImportFileForm {selectedFile = Nothing, autoSplit = False}
  returnToTimeline gui timelineModel
  maybe (ireturn timelineModel) (importAsset gui timelineModel) f
  where
    fillForm model mf = do
      updateImport gui model
      cmd <- nextEvent gui
      case (cmd, mf) of
        (CommandKeyMappedEvent Cancel, _) -> ireturn Nothing
        (CommandKeyMappedEvent Help  , _) -> do
          help gui [ModeKeyMap SImportMode (keymaps SImportMode)]
          fillForm model mf
        (ImportClicked, ImportFileForm { selectedFile = Just file, ..}) ->
          ireturn (Just (file, autoSplit))
        (ImportClicked          , form) -> fillForm model form
        (ImportFileSelected file, form) -> fillForm
          model { autoSplitValue     = False
                , autoSplitAvailable = maybe False (\f -> isSupportedVideoFile f || isSupportedAudioFile f) file
                }
          form { selectedFile = file }
        (ImportAutoSplitSet s, form) ->
          fillForm model { autoSplitValue = s } form { autoSplit = s }

data Ok = Ok deriving (Eq, Enum)

instance DialogChoice Ok where
  toButtonLabel Ok = "OK"

importAsset
  :: (UserInterface m, IxMonadIO m)
  => Name n
  -> TimelineModel
  -> (FilePath, Bool)
  -> Actions m '[n := Remain (State m TimelineMode)] r TimelineModel
importAsset gui timelineModel (filepath, autoSplit)
  | isSupportedVideoFile filepath =
    let action =
          case autoSplit of
            True ->
              importVideoFileAutoSplit
                (currentProject timelineModel ^. proxyVideoSettings)
                filepath
                (currentProject timelineModel ^. workingDirectory)
            False ->
              (: []) <$>
              importVideoFile
                (currentProject timelineModel ^. proxyVideoSettings)
                filepath
                (currentProject timelineModel ^. workingDirectory)
    in progressBar gui "Importing Video" action >>>= \case
         Nothing -> do
           ireturn timelineModel
         Just (assets :: Either VideoImportError [VideoAsset]) ->
          handleImportResult gui timelineModel SVideo assets
  | isSupportedAudioFile filepath =
    let action =
          case autoSplit of
            True ->
              importAudioFileAutoSplit
                filepath
                (currentProject timelineModel ^. workingDirectory)
            False ->
              (: []) <$>
              importAudioFile
                filepath
                (currentProject timelineModel ^. workingDirectory)
    in progressBar gui "Importing Audio" action >>>= \case
      Nothing -> ireturn timelineModel
      Just (assets :: Either AudioImportError [AudioAsset]) ->
        handleImportResult gui timelineModel SAudio assets
  | otherwise = do
    _ <-
      dialog
        gui
        "Unsupported File"
        "The file extension of the file you've selected is not supported."
        [Ok]
    ireturn timelineModel

handleImportResult
  :: (UserInterface m, IxMonadIO m, Show err)
  => Name n
  -> TimelineModel
  -> SMediaType mt
  -> Either err [Asset mt]
  -> Actions m '[n := Remain (State m TimelineMode)] r TimelineModel
handleImportResult gui model mediaType result = case (mediaType, result) of
  (_, Left err) -> do
    iliftIO (print err)
    _ <- dialog gui "Import Failed!" (show err) [Ok]
    ireturn model
  (SVideo, Right assets) ->
    model
      & projectHistory %~ edit (library . videoAssets %~ (<> assets))
      & ireturn
  (SAudio, Right assets) ->
    model
      & projectHistory %~ edit (library . audioAssets %~ (<> assets))
      & ireturn
