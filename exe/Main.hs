{-# LANGUAGE OverloadedStrings #-}

import           FastCut.Prelude

import           System.IO.Temp

import           FastCut.Application
import           FastCut.Composition
import           FastCut.Import.Video
import           FastCut.Library
import           FastCut.Project                    (Project (..))
import           FastCut.UserInterface.GtkInterface
import           Paths_fastcut

initialProject :: FilePath -> IO Project
initialProject workDir = do
  return Project
    { _projectName = "Test"
    , _timeline =
        Timeline
          ( Sequence
              ()
              ( Parallel
                  ()
                  [VideoGap () 1]
                  [AudioGap () 1]
              :| []
              )
          :| []
          )
    , _library = Library [] []
    , _workingDirectory = workDir
    }

main :: IO ()
main = do
  initialize
  cssPath <- getDataFileName "style.css"
  withSystemTempDirectory "project.fastcut" $ \workDir -> do
    p <- initialProject workDir
    runGtkUserInterface cssPath (fastcut p)
