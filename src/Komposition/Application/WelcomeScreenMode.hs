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
          userDir <- iliftIO getUserDocumentsDirectory
          chooseFile #welcome (Open Directory) "Open Project Directory" userDir >>= \case
            Just path' ->
              iliftIO (openExistingProject path') >>= \case
                Left err -> do
                  iliftIO (putStrLn ("Opening existing project failed: " <> show err :: Text))
                  inWelcomeScreenMode
                Right existingProject' -> toTimelineWithProject existingProject'
            Nothing -> inWelcomeScreenMode
        CreateNewProjectClicked ->
          prompt
            #welcome
            "New Project Name"
            "What do you want to name your new project?"
            "OK"
            PromptText >>>= \case
            Just projectName' -> do
              userDir <- iliftIO getUserDocumentsDirectory
              let defaultDir = userDir
              chooseFile #welcome (Save Directory) "Choose Project Directory" defaultDir >>= \case
                Just path' ->
                  iliftIO (createNewProject path' (initialProject & projectName .~ projectName')) >>= \case
                    Left err -> do
                      beep #welcome
                      iliftIO (putStrLn ("Create new project failed: " <> show err :: Text))
                      inWelcomeScreenMode
                    Right newProject -> toTimelineWithProject newProject
                Nothing -> inWelcomeScreenMode
            Nothing -> inWelcomeScreenMode
        WindowClosed -> destroyWindow #welcome
        CommandKeyMappedEvent Cancel -> destroyWindow #welcome
        CommandKeyMappedEvent Help -> do
          help #welcome [ModeKeyMap STimelineMode (keymaps STimelineMode)] >>>= \case
            Just HelpClosed -> inWelcomeScreenMode
            Nothing -> inWelcomeScreenMode

toTimelineWithProject
  :: Application t m sig
  => ExistingProject
  -> t m ("welcome" .== Window (t m) (Event WelcomeScreenMode)) Empty ()
toTimelineWithProject project = do
  let model = TimelineModel project initialFocus Nothing (ZoomLevel 1)
  destroyWindow #welcome
  newWindow
    #timeline
    (timelineView model)
    (CommandKeyMappedEvent <$> keymaps STimelineMode)
  runTimeline model
  where
    runTimeline model =
      timelineMode #timeline model >>= \case
        TimelineExit -> destroyWindow #timeline
        TimelineClose -> do
          destroyWindow #timeline
          welcomeScreenMode

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
