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
  let
    gap1s = Gap () 1

  return Project
    { _projectName = "Test"
    , _timeline =
        Timeline
          ()
          ( Sequence
              ()
              ( Parallel
                  ()
                  [gap1s]
                  [gap1s]
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
