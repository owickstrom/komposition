{-# LANGUAGE ConstraintKinds     #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE OverloadedLabels    #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE PolyKinds           #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE RebindableSyntax    #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}
module Komposition.Application.ImportMode
  ( ImportError(..)
  , ImportFileForm(..)
  , selectFileToImport
  , importSelectedFile
  ) where

import           Komposition.Application.Base

import           Control.Effect                   (Member)
import           Control.Lens
import           Data.Row.Records
import           Data.String                      (fromString)

import           Komposition.Application.Form
import           Komposition.Classification
import           Komposition.History
import           Komposition.Import.Audio
import           Komposition.Import.Video
import           Komposition.Library
import           Komposition.MediaType
import           Komposition.Project
import           Komposition.UserInterface.Dialog
import           Komposition.UserInterface.Help
import           Komposition.VideoSpeed

import           Komposition.Application.KeyMaps

type ImportEffects sig = (Member AudioImport sig, Member VideoImport sig)

newtype ImportError = ImportError SomeException
  deriving (Show)

data ImportFileForm f = ImportFileForm
  { selectedFile      :: FormData f FilePath
  , classify          :: FormData Valid Bool
  , defaultVideoSpeed :: FormData Valid VideoSpeed
  }

selectFileToImport
  :: ( Application t m sig
    , ImportEffects sig
    )
  => t m r r (Maybe (ImportFileForm Valid))
selectFileToImport =
  let initialModel = ImportFileModel { classifyValue = False
                                     , classifyAvailable = False
                                     , setDefaultVideoSpeed = VideoSpeed 1.0
                                     , selectedFileMediaType = Nothing
                                     }
      initialForm = ImportFileForm { selectedFile = Nothing
                                   , classify = False
                                   , defaultVideoSpeed = VideoSpeed 1.0
                                   }
  in
    withNewWindow
      #import
      (importView initialModel)
      (CommandKeyMappedEvent <$> keymaps SImportMode)
      (fillForm initialModel initialForm)
  where
    fillForm
      :: ( Application t m sig
      , ImportEffects sig
      , r ~ ("import" .== Window (t m) (Event ImportMode))
      )
      => ImportFileModel -> ImportFileForm Maybe -> t m r r (Maybe (ImportFileForm Valid))
    fillForm model mf = do
      patchWindow #import (importView model)
      cmd <- nextEvent #import
      case (cmd, mf) of
        (CommandKeyMappedEvent Help  , _) ->
          help #import [ModeKeyMap SImportMode (keymaps SImportMode)] >>>= \case
            Just HelpClosed -> fillForm model mf
            Nothing -> fillForm model mf
        (ImportClicked, ImportFileForm { selectedFile = Just file, ..}) ->
          ireturn (Just ImportFileForm { selectedFile = file, .. })
        (ImportClicked          , form) -> fillForm model form
        (ImportFileSelected file, form) -> do
          fileMediaType <-
            case file of
                Just f  -> getImportableFileMediaType f
                Nothing -> ireturn Nothing
          fillForm
            model { classifyAvailable = isJust fileMediaType
                  , selectedFileMediaType = fileMediaType
                  }
            form { selectedFile = file }
        (ImportClassifySet s, form) ->
          fillForm model { classifyValue = s } form { classify = s }
        (ImportDefaultVideoSpeedChanged s, form) ->
          fillForm model { setDefaultVideoSpeed = s } form { defaultVideoSpeed = s }
        (CommandKeyMappedEvent Cancel, _) -> ireturn Nothing
        (WindowClosed, _) -> ireturn Nothing

importSelectedFile
  :: ( Application t m sig
     , r ~ (n .== Window (t m) e)
     , DialogView (WindowMarkup (t m))
     , ImportEffects sig
     , Typeable e
     )
  => Name n
  -> ExistingProject
  -> ImportFileForm Valid
  -> t
       m
       r
       r
       ( Maybe
           (Either ImportError (Either [VideoAsset] [AudioAsset]))
       )
importSelectedFile gui project ImportFileForm{selectedFile, classify, defaultVideoSpeed} = do
  v <- ilift (isSupportedVideoFile selectedFile)
  a <- ilift (isSupportedAudioFile selectedFile)
  let classification = bool Unclassified Classified classify
  case (v, a) of
    (True, _) -> do
      action <-
        ilift $
        importVideoFile
          classification
          (current (project ^. projectHistory) ^. videoSettings)
          defaultVideoSpeed
          selectedFile
          (project ^. projectPath . unProjectPath)
      result <- progressBar gui "Importing Video" action
      ireturn (bimap ImportError Left <$> result)
    (False, True) -> do
      action <-
        ilift $
        importAudioFile
          classification
          selectedFile
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

getImportableFileMediaType
  :: (ImportEffects sig, Application t m sig) => FilePath -> t m r r (Maybe MediaType)
getImportableFileMediaType f = do
  v <- ilift (isSupportedVideoFile f)
  a <- ilift (isSupportedAudioFile f)
  case (v, a) of
    (True, _) -> ireturn (Just Video)
    (_, True) -> ireturn (Just Audio)
    (_, _)    -> ireturn Nothing
