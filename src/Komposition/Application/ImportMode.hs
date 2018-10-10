{-# LANGUAGE ConstraintKinds     #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
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
module Komposition.Application.ImportMode
  ( ImportError(..)
  , selectFileToImport
  , importSelectedFile
  ) where

import           Komposition.Application.Base

import           Control.Lens
import           Data.Row.Records
import           Data.String                     (fromString)

import           Komposition.History
import           Komposition.Import.Audio
import           Komposition.Import.Video
import           Komposition.Library
import           Komposition.Project

import           Komposition.Application.KeyMaps

data ImportError
  = VideoImportError VideoImportError
  | AudioImportError AudioImportError
  deriving (Eq, Show)

data ImportFileForm = ImportFileForm
  { selectedFile :: Maybe FilePath
  , autoSplit    :: Bool
  }

selectFileToImport
  :: ( Application t m)
  => Name n
  -> ThroughMode TimelineMode ImportMode (t m) n (Maybe (FilePath, Bool))
selectFileToImport gui returnToOrigin = do
  let initialModel = ImportFileModel {autoSplitValue = False, autoSplitAvailable = True}
  enterImport gui initialModel
  result <- fillForm initialModel ImportFileForm {selectedFile = Nothing, autoSplit = False}
  case result of
    Just f -> returnToOrigin (Just f)
    Nothing -> returnToOrigin Nothing
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

importSelectedFile
  :: (UserInterface m, IxMonadIO m, (r .! n) ~ State m s)
  => Name n
  -> ExistingProject
  -> (FilePath, Bool)
  -> m r r (Maybe (Either ImportError (Either [VideoAsset] [AudioAsset])))
importSelectedFile gui project (filepath, autoSplit)
  | isSupportedVideoFile filepath = do
    let action =
          case autoSplit of
            True ->
              importVideoFileAutoSplit
                (current (project ^. projectHistory) ^. proxyVideoSettings)
                filepath
                (project ^. projectPath . unProjectPath)
            False ->
              (: []) <$>
              importVideoFile
                (current (project ^. projectHistory) ^. proxyVideoSettings)
                filepath
                (project ^. projectPath . unProjectPath)
    result <- progressBar gui "Importing Video" action
    ireturn (bimap VideoImportError Left <$> result)
  | isSupportedAudioFile filepath = do
    let action =
          case autoSplit of
            True ->
              importAudioFileAutoSplit
                filepath
                (project ^. projectPath . unProjectPath)
            False ->
              (: []) <$>
              importAudioFile
                filepath
                (project ^. projectPath . unProjectPath)
    result <- progressBar gui "Importing Audio" action
    ireturn (bimap AudioImportError Right <$> result)
  | otherwise = do
    _ <-
      dialog
        gui
        "Unsupported File"
        "The file extension of the file you've selected is not supported."
        [Ok]
    ireturn Nothing
