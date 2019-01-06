{-# LANGUAGE OverloadedLabels  #-}
{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE OverloadedStrings #-}

-- |
module Komposition.UserInterface.GtkInterface.VideoSpeedControl where

import           Komposition.Prelude

import           Control.Lens
import           GI.Gtk.Declarative

import           Komposition.UserInterface.GtkInterface.NumberInput
import           Komposition.VideoSpeed

videoSpeedControl :: VideoSpeed -> Widget VideoSpeed
videoSpeedControl current =    toDefaultVideoSpeedChanged
  <$> numberInput NumberInputProperties
        { value              = current ^. unVideoSpeed
        , range              = (0.1, 10.0)
        , step               = 0.1
        , digits             = 1
        , numberInputClasses = []
        }
  where toDefaultVideoSpeedChanged (NumberInputChanged v) = VideoSpeed v
