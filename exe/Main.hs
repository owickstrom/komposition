{-# LANGUAGE OverloadedStrings #-}

import           FastCut.Prelude

import           System.IO.Temp

import           FastCut.Application
import           FastCut.Composition
import           FastCut.Import.Video
import           FastCut.Library
import           FastCut.Project
import           FastCut.UserInterface.GtkInterface
import           FastCut.VideoSettings
import           Paths_fastcut

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
    }

main :: IO ()
main = do
  initialize
  cssPath <- getDataFileName "style.css"
  withSystemTempDirectory "project.fastcut" $ \workDir -> do
    p <- initialProject workDir
    runGtkUserInterface cssPath (fastcut p)
