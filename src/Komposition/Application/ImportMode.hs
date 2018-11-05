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

import           Control.Effect                  (Member)
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
  , classify     :: Bool
  }

selectFileToImport
  :: (Member VideoImport sig)
  => Application t m sig
  => Name n
  -> ThroughMode TimelineMode ImportMode (t m) n (Maybe (FilePath, Bool))
selectFileToImport gui returnToOrigin = do
  let initialModel = ImportFileModel {autoSplitValue = False, autoSplitAvailable = True}
  enterImport gui initialModel
  result <- fillForm initialModel ImportFileForm {selectedFile = Nothing, classify = False}
  case result of
    Just f  -> returnToOrigin (Just f)
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
          ireturn (Just (file, classify))
        (ImportClicked          , form) -> fillForm model form
        (ImportFileSelected file, form) -> do
          isClassificationAvailable <-
            case file of
                Just f  -> isImportable f
                Nothing -> ireturn False
          fillForm
            model { autoSplitValue     = False
                  , autoSplitAvailable = isClassificationAvailable
                  }
            form { selectedFile = file }
        (ImportAutoSplitSet s, form) ->
          fillForm model { autoSplitValue = s } form { classify = s }

isImportable
  :: (Member VideoImport sig, Application t m sig) => FilePath -> t m r r Bool
isImportable f = do
  v <- ilift (isSupportedVideoFile f)
  a <- iliftIO (isSupportedAudioFile f)
  ireturn (v || a)

importSelectedFile
  :: (Member VideoImport sig, Application t m sig, (r .! n) ~ State (t m) s)
  => Name n
  -> ExistingProject
  -> (FilePath, Bool)
  -> t
       m
       r
       r
       ( Maybe
           (Either ImportError (Either [VideoAsset] [AudioAsset]))
       )
importSelectedFile gui project (filepath, classify) = do
  v <- ilift (isSupportedVideoFile filepath)
  a <- iliftIO (isSupportedAudioFile filepath)
  let classification = bool Classified Unclassified classify
  case (v, a) of
    (True, _) -> do
      action <-
            ilift $
              importVideoFile
                classification
                (current (project ^. projectHistory) ^. proxyVideoSettings)
                filepath
                (project ^. projectPath . unProjectPath)
      result <- progressBar gui "Importing Video" action
      ireturn (bimap VideoImportError Left <$> result)
    (False, True) -> do
      let action =
            case classification of
              Classified ->
                importAudioFileAutoSplit
                  filepath
                  (project ^. projectPath . unProjectPath)
              Unclassified ->
                (: []) <$>
                importAudioFile
                  filepath
                  (project ^. projectPath . unProjectPath)
      result <- progressBar gui "Importing Audio" action
      ireturn (bimap AudioImportError Right <$> result)
    _ -> do
      _ <-
        dialog
          gui
          "Unsupported File"
          "The file extension of the file you've selected is not supported."
          [Ok]
      ireturn Nothing
