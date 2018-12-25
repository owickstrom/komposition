{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE OverloadedLabels  #-}
{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

-- | The import view of Komposition's GTK interface.
module Komposition.UserInterface.GtkInterface.ImportView
  ( importView
  ) where

import           Control.Lens
import           Komposition.Prelude                                hiding
                                                                     (State, on)

import           GI.Gtk                                             (Align (..),
                                                                     Box (..),
                                                                     Button (..),
                                                                     CheckButton (..),
                                                                     FileChooserButton (..),
                                                                     Label (..),
                                                                     Orientation (..),
                                                                     Window (..),
                                                                     fileChooserGetFilename,
                                                                     toggleButtonGetActive)
import           GI.Gtk.Declarative                                 as Gtk

import           Komposition.MediaType
import           Komposition.UserInterface                          hiding
                                                                     (Window,
                                                                     importView)
import           Komposition.UserInterface.GtkInterface.NumberInput
import           Komposition.VideoSpeed

importView :: ImportFileModel -> Bin Window Widget (Event ImportMode)
importView ImportFileModel {..} =
  bin
    Window
    [ #title := "Import File"
    , on #deleteEvent (const (True, WindowClosed))
    , #defaultWidth := 300
    ] $
  container Box [classes ["import-view"], #orientation := OrientationVertical] $
    mconcat
    [
      [boxChild False False 10 $
        widget
          FileChooserButton
          [ onM
              #selectionChanged
              (fmap ImportFileSelected . fileChooserGetFilename)
          ]]
    , mediaTypeSpecificSettings
    , [boxChild False False 10 $
        widget Button [#label := "Import", on #clicked ImportClicked]]
    ]
  where
    mediaTypeSpecificSettings :: [BoxChild (Event ImportMode)]
    mediaTypeSpecificSettings =
      case selectedFileMediaType of
        Just Video -> classifyCheckBox : videoSpeedControl
        Just Audio -> [classifyCheckBox]
        Nothing    -> []
    classifyCheckBox =
      boxChild False False 10 $
        widget
          CheckButton
          [ #label := "Classify parts automatically"
          , #active := classifyValue
          , #sensitive := classifyAvailable
          , onM #toggled (fmap ImportClassifySet . toggleButtonGetActive)
          ]
    videoSpeedControl =
      [ boxChild False False 5 $
        widget Label [#label := "Video Speed", #halign := AlignStart]
      , boxChild False False 5 $
        toDefaultVideoSpeedChanged <$>
        numberInput NumberInputProperties{ value = setDefaultVideoSpeed ^. unVideoSpeed
                                         , range = (0.1, 10.0)
                                         , step = 0.1
                                         , digits = 1
                                         , numberInputClasses = []
                                         }
      ]
      where
        toDefaultVideoSpeedChanged (NumberInputChanged v) = ImportDefaultVideoSpeedChanged (VideoSpeed v)
