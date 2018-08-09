{-# LANGUAGE OverloadedStrings #-}

import           FastCut.Prelude

import           FastCut.Application
import           FastCut.Composition
import           FastCut.Library
import           FastCut.Project                    (Project (..))
import           FastCut.UserInterface.GtkInterface
import           FastCut.Video.FFmpeg
import           Paths_fastcut

initialProject :: IO Project
initialProject = do
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
    }

main :: IO ()
main = do
  initialize
  cssPath <- getDataFileName "style.css"
  p <- initialProject
  withGtkUserInterface cssPath runFFmpegImporterT (fastcut p)
