{-# LANGUAGE OverloadedStrings #-}

import           Komposition.Prelude

import           System.IO.Temp

import           Komposition.Application
import           Komposition.Composition
import           Komposition.Import.Video
import           Komposition.Library
import           Komposition.Project
import           Komposition.UserInterface.GtkInterface
import           Komposition.VideoSettings
import           Paths_komposition

initialProject :: FilePath -> IO Project
initialProject workDir = do
  return
    Project
    { _projectName = "Test"
    , _timeline = emptyTimeline
    , _library = Library [] []
    , _workingDirectory = workDir
    , _videoSettings =
        VideoSettings {_frameRate = 25, _resolution = Resolution 1920 1080}
    , _proxyVideoSettings =
        VideoSettings {_frameRate = 25, _resolution = Resolution 1280 720}
    }

main :: IO ()
main = do
  initialize
  cssPath <- getDataFileName "style.css"
  withSystemTempDirectory "project.komposition" $ \workDir -> do
    p <- initialProject workDir
    runGtkUserInterface cssPath (komposition p)
