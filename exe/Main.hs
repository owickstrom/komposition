{-# LANGUAGE OverloadedStrings #-}

import           Komposition.Prelude

import           Komposition.Application
import           Komposition.Import.Video.FFmpeg
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
  let runEffects = runFastLoggerLog loggerSet . runFileProjectStoreIO . runFFmpegVideoImport
  runGtkUserInterface cssPath runEffects komposition
