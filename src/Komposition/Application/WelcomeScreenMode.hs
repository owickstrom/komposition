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
import           Control.Lens
import qualified Data.List.NonEmpty                   as NonEmpty
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
import           Komposition.UserInterface.Dialog
import           Komposition.VideoSettings

import           Komposition.Application.KeyMaps
import           Komposition.Application.TimelineMode
import           Komposition.UserInterface.Help

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
  => t m Empty Empty ()
welcomeScreenMode = do
  newWindow #welcome welcomeView (CommandKeyMappedEvent <$> keymaps SWelcomeScreenMode)
  inWelcomeScreenMode
  where
    inWelcomeScreenMode = do
      patchWindow #welcome welcomeView
      nextEvent #welcome >>= \case
        OpenExistingProjectClicked -> do
          dir <- ilift getDefaultProjectsDirectory
          chooseFile #welcome (Open Directory) "Open Project Directory" dir >>= \case
            Just path' ->
              ilift (openExistingProject path') >>= \case
                Left err -> do
                  ilift (logLnText Error ("Opening existing project failed: " <> show err))
                  inWelcomeScreenMode
                Right existingProject' -> toTimelineWithProject existingProject'
            Nothing -> inWelcomeScreenMode
        CreateNewProjectClicked ->
          editNewProject initialNewProjectModel >>>= \case
            Just model -> do
              defaultDir <- ilift getDefaultProjectsDirectory
              chooseFile #welcome (Save Directory) "Choose Project Directory" defaultDir >>= \case
                Just path' -> do
                  let customizedProject =
                        initialProject
                          & projectName .~ (model ^. newProjectName)
                          & videoSettings . renderVideoSettings .~ VideoSettings (model ^. newProjectFrameRate) (model ^. newProjectResolution)
                  ilift (createNewProject path' customizedProject) >>= \case
                    Left err -> do
                      beep #welcome
                      ilift (logLnText Error ("Create new project failed: " <> show err))
                      inWelcomeScreenMode
                    Right newProject -> toTimelineWithProject newProject
                Nothing -> inWelcomeScreenMode
            Nothing -> inWelcomeScreenMode
        WindowClosed -> destroyWindow #welcome
        CommandKeyMappedEvent Cancel -> destroyWindow #welcome
        CommandKeyMappedEvent Help ->
          help #welcome [ModeKeyMap STimelineMode (keymaps STimelineMode)] >>>= \case
            Just HelpClosed -> inWelcomeScreenMode
            Nothing -> inWelcomeScreenMode

toTimelineWithProject
  :: ( Application t m sig
    , WelcomeScreenModeEffects sig
    , Carrier sig m
    )
  => ExistingProject
  -> t m ("welcome" .== Window (t m) (Event WelcomeScreenMode)) Empty ()
toTimelineWithProject project = do
  let model = TimelineModel project initialFocus Nothing (ZoomLevel 1) Nothing
  destroyWindow #welcome
  newWindow
    #timeline
    (timelineView model)
    (CommandKeyMappedEvent <$> keymaps STimelineMode)
  runTimeline model
  where
    runTimeline model =
      timelineMode #timeline model >>= \case
        TimelineExit model' ->
          dialog #timeline DialogProperties { dialogTitle = "Confirm Exit", dialogMessage = "Are you sure you want to exit?", dialogChoices = [No, Yes]} >>>= \case
            Just Yes -> destroyWindow #timeline
            Just No -> runTimeline model'
            Nothing -> runTimeline model'
        TimelineClose -> do
          destroyWindow #timeline
          welcomeScreenMode

initialProject :: Project
initialProject =
  Project
    { _projectName = "Test"
    , _timeline = emptyTimeline
    , _library = Library [] []
    , _videoSettings = AllVideoSettings
                       { _renderVideoSettings =
                         VideoSettings {_frameRate = 25, _resolution = Resolution 1920 1080}
                       , _proxyVideoSettings =
                         VideoSettings {_frameRate = 25, _resolution = Resolution 960 540}
                       }
    }

initialFocus :: Focus SequenceFocusType
initialFocus = SequenceFocus 0 Nothing

initialNewProjectModel :: NewProjectModel
initialNewProjectModel =
  NewProjectModel "Untitled" 25 (NonEmpty.head resolutions)

editNewProject
  :: ( Application t m sig
    , HasType "welcome" (Window (t m) (Event WelcomeScreenMode)) r
    )
  => NewProjectModel -> t m r r (Maybe NewProjectModel)
editNewProject initialModel =
  withNewModalWindow
    #welcome
    #newProject
    (newProjectView initialModel)
    (CommandKeyMappedEvent <$> keymaps SNewProjectMode)
    (go initialModel)
  where
    go
      :: ( Application t m sig
        , r ~ ("newProject" .== Window (t m) (Event NewProjectMode))
        )
      => NewProjectModel
      -> t m r r (Maybe NewProjectModel)
    go model =
      nextEvent #newProject >>>= \case
        ProjectNameChanged n -> go (model & newProjectName .~ n)
        FrameRateChanged fr -> go (model & newProjectFrameRate .~ fr )
        ResolutionChanged r -> go (model & newProjectResolution .~ r )
        CreateClicked -> ireturn (Just model)
        WindowClosed -> ireturn Nothing
        CommandKeyMappedEvent Cancel -> ireturn Nothing
        CommandKeyMappedEvent Help ->
          help #newProject [ModeKeyMap STimelineMode (keymaps STimelineMode)] >>>= \case
            Just HelpClosed -> go model
            Nothing -> go model
