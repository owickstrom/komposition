{-# OPTIONS_GHC -fno-warn-unticked-promoted-constructors #-}

{-# LANGUAGE ConstraintKinds   #-}
{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedLabels  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds         #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE RebindableSyntax  #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeOperators     #-}
module FastCut.Application.ImportMode where

import           FastCut.Application.Base

import           Control.Lens
import           Data.String                 (fromString)

import           FastCut.Import.Audio
import           FastCut.Import.Video
import           FastCut.Library
import           FastCut.MediaType
import           FastCut.Project

import           FastCut.Application.KeyMaps

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
                , autoSplitAvailable = maybe False isSupportedVideoFile file
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
                (timelineModel ^. project . videoSettings)
                filepath
                (timelineModel ^. project . workingDirectory)
            False ->
              fmap (: []) <$>
              importVideoFile
                filepath
                (timelineModel ^. project . workingDirectory)
    in progressBar gui "Import Video" action >>>= \case
         Nothing -> do
           ireturn timelineModel
         Just assets -> handleImportResult gui timelineModel SVideo assets
  | isSupportedAudioFile filepath =
    progressBar
      gui
      "Import Video"
      (importAudioFile filepath (timelineModel ^. project . workingDirectory)) >>>= \case
      Nothing -> ireturn timelineModel
      Just asset ->
        handleImportResult gui timelineModel SAudio (fmap pure asset)
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
  (SVideo, Right assets) -> do
    model & project . library . videoAssets %~ (<> assets) & ireturn
  (SAudio, Right assets) -> do
    model & project . library . audioAssets %~ (<> assets) & ireturn
