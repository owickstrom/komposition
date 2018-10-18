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

import           Data.Row.Records                     hiding (split)
import           Data.String                          (fromString)
import           System.Directory

import           Komposition.Composition
import           Komposition.Focus
import           Komposition.Library
import           Komposition.Project
import           Komposition.Project.Store
import           Komposition.UserInterface.Dialog
import           Komposition.VideoSettings

import           Komposition.Application.KeyMaps
import           Komposition.Application.TimelineMode

welcomeScreenMode
  :: DialogView (WindowMarkup (t m))
  => Application t m
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
        CreateNewProjectClicked -> do
          userDir <- iliftIO getUserDocumentsDirectory
          chooseFile #welcome (Save Directory) "Choose Project Directory" userDir >>= \case
            Just path' ->
              iliftIO (createNewProject path' initialProject) >>= \case
                Left err -> do
                  beep #welcome
                  iliftIO (putStrLn ("Create new project failed: " <> show err :: Text))
                  inWelcomeScreenMode
                Right newProject -> toTimelineWithProject newProject
            Nothing -> inWelcomeScreenMode
        WindowClosed -> destroyWindow #welcome
        CommandKeyMappedEvent Cancel -> destroyWindow #welcome
        CommandKeyMappedEvent Help -> do
          help [ModeKeyMap STimelineMode (keymaps STimelineMode)]
          inWelcomeScreenMode

toTimelineWithProject
  :: DialogView (WindowMarkup (t m))
  => Application t m
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
