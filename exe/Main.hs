{-# LANGUAGE OverloadedStrings #-}

import           Komposition.Prelude

import           Komposition.Application
import           Komposition.Import.Video
import           Komposition.Logging.FastLogger
import           Komposition.Project.Store.File
import           Komposition.UserInterface.GtkInterface
import           Paths_komposition
import           System.Log.FastLogger

main :: IO ()
main = do
  initialize
  cssPath   <- getDataFileName "style.css"
  loggerSet <- newStderrLoggerSet 1024
  runGtkUserInterface cssPath
                      (runFastLoggerLog loggerSet . runFileProjectStoreIO)
                      komposition
