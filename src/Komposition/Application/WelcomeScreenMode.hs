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
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}
module Komposition.Application.WelcomeScreenMode where

import           Komposition.Application.Base

import           Control.Effect                       (Member)
import           Control.Effect.Carrier               (Carrier)
import           Data.Row.Records                     hiding (split)
import           Data.String                          (fromString)

import           Komposition.Composition
import           Komposition.Focus
import           Komposition.Import.Audio
import           Komposition.Import.Video
import           Komposition.Library
import           Komposition.Project
import           Komposition.Project.Store
import           Komposition.Render
import           Komposition.VideoSettings

import           Komposition.Application.KeyMaps
import           Komposition.Application.TimelineMode

type WelcomeScreenModeEffects sig =
    ( Member ProjectStore sig
    , Member VideoImport sig
    , Member AudioImport sig
    , Member Render sig
    )

welcomeScreenMode
  :: ( Application t m sig
    , WelcomeScreenModeEffects sig
    , Carrier sig m
    )
  => Name n
  -> t m (n .== State (t m) WelcomeScreenMode) Empty ()
welcomeScreenMode gui = do
  updateWelcomeScreen gui
  nextEvent gui >>= \case
    OpenExistingProjectClicked -> do
      outDir <- ilift getDefaultProjectsDirectory
      chooseFile gui (Open Directory) "Open Project Directory" outDir >>= \case
        Just path' ->
          ilift (openExistingProject path') >>= \case
            Left err -> do
              ilift (logLnText Error ("Opening existing project failed: " <> show err))
              welcomeScreenMode gui
            Right existingProject' -> toTimelineWithProject gui existingProject'
        Nothing -> welcomeScreenMode gui
    CreateNewProjectClicked -> do
      outDir <- ilift getDefaultProjectsDirectory
      chooseFile gui (Save Directory) "Choose Project Directory" outDir >>= \case
        Just path' ->
          ilift (createNewProject path' initialProject) >>= \case
            Left err -> do
              beep gui
              ilift (logLnText Error ("Create new project failed: " <> show err))
              welcomeScreenMode gui
            Right newProject -> toTimelineWithProject gui newProject
        Nothing -> welcomeScreenMode gui
    CommandKeyMappedEvent Cancel -> exit gui
    CommandKeyMappedEvent Help -> do
      help gui [ModeKeyMap STimelineMode (keymaps STimelineMode)]
      welcomeScreenMode gui

toTimelineWithProject
  :: ( Application t m sig
    , Carrier sig m
    , WelcomeScreenModeEffects sig
    )
  => Name n
  -> ExistingProject
  -> t m (n .== State (t m) WelcomeScreenMode) Empty ()
toTimelineWithProject gui project = do
  let model = TimelineModel project initialFocus Nothing (ZoomLevel 1)
  returnToTimeline gui model
  runTimeline model
  where
    runTimeline model =
      timelineMode gui model >>= \case
        TimelineExit model' ->
          dialog gui "Confirm Exit" "Are you sure you want to exit?" [No, Yes] >>>= \case
            Just Yes -> exit gui
            Just No -> runTimeline model'
            Nothing -> runTimeline model'
        TimelineClose -> returnToWelcomeScreen gui >>> welcomeScreenMode gui

data Confirmation
  = Yes
  | No
  deriving (Show, Eq, Enum)

instance DialogChoice Confirmation where
  toButtonLabel = \case
    Yes -> "Yes"
    No -> "No"

initialProject :: Project
initialProject =
  Project
    { _projectName = "Test"
    , _timeline = emptyTimeline
    , _library = Library [] []
    , _videoSettings =
        VideoSettings {_frameRate = 25, _resolution = Resolution 1920 1080}
    , _proxyVideoSettings =
        VideoSettings {_frameRate = 25, _resolution = Resolution 960 540}
    }

initialFocus :: Focus SequenceFocusType
initialFocus = SequenceFocus 0 Nothing
