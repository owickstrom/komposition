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

import           Control.Effect                   (Member)
import           Control.Lens
import           Data.Row.Records
import           Data.String                      (fromString)

import           Komposition.Classification
import           Komposition.History
import           Komposition.Import.Audio
import           Komposition.Import.Video
import           Komposition.Library
import           Komposition.Project
import           Komposition.UserInterface.Dialog
import           Komposition.UserInterface.Help

import           Komposition.Application.KeyMaps

type ImportEffects sig = (Member AudioImport sig, Member VideoImport sig)

newtype ImportError = ImportError SomeException
  deriving (Show)

data ImportFileForm = ImportFileForm
  { selectedFile :: Maybe FilePath
  , classify     :: Bool
  }

selectFileToImport
  :: ( Application t m sig
    , ImportEffects sig
    )
  => t m r r (Maybe (FilePath, Bool))
selectFileToImport =
  let initialModel = ImportFileModel {autoSplitValue = False, autoSplitAvailable = True}
  in
    withNewWindow
      #import
      (importView initialModel)
      (CommandKeyMappedEvent <$> keymaps SImportMode)
      (fillForm initialModel ImportFileForm {selectedFile = Nothing, classify = False})
  where
    fillForm model mf = do
      patchWindow #import (importView model)
      cmd <- nextEvent #import
      case (cmd, mf) of
        (CommandKeyMappedEvent Help  , _) ->
          help #import [ModeKeyMap SImportMode (keymaps SImportMode)] >>>= \case
            Just HelpClosed -> fillForm model mf
            Nothing -> fillForm model mf
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
        (CommandKeyMappedEvent Cancel, _) -> ireturn Nothing
        (WindowClosed, _) -> ireturn Nothing

importSelectedFile
  :: ( Application t m sig
     , r ~ (n .== Window (t m) e)
     , DialogView (WindowMarkup (t m))
     , ImportEffects sig
     )
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
  a <- ilift (isSupportedAudioFile filepath)
  let classification = bool Unclassified Classified classify
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
      ireturn (bimap ImportError Left <$> result)
    (False, True) -> do
      action <-
        ilift $
        importAudioFile
          classification
          filepath
          (project ^. projectPath . unProjectPath)
      result <- progressBar gui "Importing Audio" action
      ireturn (bimap ImportError Right <$> result)
    _ -> do
      _ <-
        dialog
          gui
          DialogProperties
          { dialogTitle = "Unsupported File"
          , dialogMessage =
              "The file extension of the file you've selected is not supported."
          , dialogChoices = [Ok]
          }
      ireturn Nothing

isImportable
  :: (ImportEffects sig, Application t m sig) => FilePath -> t m r r Bool
isImportable f = do
  v <- ilift (isSupportedVideoFile f)
  a <- ilift (isSupportedAudioFile f)
  ireturn (v || a)
