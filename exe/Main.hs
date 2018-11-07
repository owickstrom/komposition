{-# LANGUAGE OverloadedStrings #-}

import           Komposition.Prelude

import           Komposition.Application
import           Komposition.Import.Audio.Sox
import           Komposition.Import.Video.FFmpeg
import           Komposition.Logging.FastLogger
import           Komposition.Project.Store.File
import           Komposition.Render.FFmpeg
import           Komposition.UserInterface.GtkInterface
import           Komposition.UserInterface.GtkInterface.DialogView ()
import           Komposition.UserInterface.GtkInterface.HelpView ()
import           Paths_komposition
import           System.Log.FastLogger

main :: IO ()
main = do
  initialize
  cssPath   <- getDataFileName "style.css"
  loggerSet <- newStderrLoggerSet 1024
  let runEffects =
        runFastLoggerLog loggerSet
          . runFileProjectStoreIO
          . runFFmpegRender
          . runFFmpegVideoImport
          . runSoxAudioImport
  runGtkUserInterface cssPath runEffects komposition
