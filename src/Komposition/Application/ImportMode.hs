{-# LANGUAGE ConstraintKinds     #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
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
  :: ( Application t m
     )
  => t m r r (Maybe (FilePath, Bool))
selectFileToImport =
  let initialModel = ImportFileModel {autoSplitValue = False, autoSplitAvailable = True}
  in
    withNewWindow
      #import
      (importView initialModel)
      (CommandKeyMappedEvent <$> keymaps SImportMode)
      (fillForm initialModel ImportFileForm {selectedFile = Nothing, autoSplit = False})
  where
    fillForm model mf = do
      patchWindow #import (importView model)
      cmd <- nextEvent #import
      case (cmd, mf) of
        (CommandKeyMappedEvent Help  , _) -> do
          help [ModeKeyMap SImportMode (keymaps SImportMode)]
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
        (CommandKeyMappedEvent Cancel, _) -> ireturn Nothing
        (WindowClosed, _) -> ireturn Nothing

importSelectedFile
  :: ( Application t m
     , r ~ (n .== Window (t m) e)
     )
  => Name n
  -> ExistingProject
  -> (FilePath, Bool)
  -> t m r r (Maybe (Either ImportError (Either [VideoAsset] [AudioAsset])))
importSelectedFile parent project (filepath, autoSplit)
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
    result <- progressBar parent "Importing Video" action
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
    result <- progressBar parent "Importing Audio" action
    ireturn (bimap AudioImportError Right <$> result)
  | otherwise = do
    _ <-
      dialog
        "Unsupported File"
        "The file extension of the file you've selected is not supported."
        [Ok]
    ireturn Nothing
