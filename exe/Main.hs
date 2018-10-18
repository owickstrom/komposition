{-# LANGUAGE OverloadedStrings #-}

import           Komposition.Prelude

import           Komposition.Application
import           Komposition.Import.Video
import           Komposition.UserInterface.GtkInterface
import           Komposition.UserInterface.GtkInterface.DialogView ()
import           Paths_komposition

main :: IO ()
main = do
  initialize
  cssPath <- getDataFileName "style.css"
  runGtkUserInterface cssPath komposition
