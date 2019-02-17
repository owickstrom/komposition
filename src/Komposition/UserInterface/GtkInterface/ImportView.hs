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
import           Komposition.Prelude                                      hiding (State,
                                                                           on)

import           Data.Vector                                              (Vector)
import           GI.Gtk                                                   (Align (..),
                                                                           Box (..),
                                                                           Button (..),
                                                                           CheckButton (..),
                                                                           Dialog (..),
                                                                           FileChooserButton (..),
                                                                           Label (..),
                                                                           Orientation (..),
                                                                           fileChooserGetFilename,
                                                                           toggleButtonGetActive)
import           GI.Gtk.Declarative                                       as Gtk

import           Komposition.MediaType
import           Komposition.UserInterface                                hiding (Window,
                                                                           importView)
import           Komposition.UserInterface.GtkInterface.VideoSpeedControl

importView :: ImportFileModel -> Bin Dialog (Event 'ImportMode)
importView ImportFileModel {..} =
  bin
    Dialog
    [ #title := "Import File"
    , on #deleteEvent (const (True, WindowClosed))
    , #defaultWidth := 300
    ] $
  container Box [classes ["import-view"], #orientation := OrientationVertical] $
    mconcat
    [
      [BoxChild defaultBoxChildProperties { expand = False, fill = False, padding = 10 } $
        widget
          FileChooserButton
          [ onM
              #selectionChanged
              (fmap ImportFileSelected . fileChooserGetFilename)
          ]]
    , mediaTypeSpecificSettings
    , [BoxChild defaultBoxChildProperties { expand = False, fill = False, padding = 10 } $
        widget Button [#label := "Import", on #clicked ImportClicked]]
    ]
  where
    mediaTypeSpecificSettings :: Vector (BoxChild (Event 'ImportMode))
    mediaTypeSpecificSettings =
      case selectedFileMediaType of
        Just Video -> [classifyCheckBox, defaultVideoSpeedControl]
        Just Audio -> [classifyCheckBox]
        Nothing    -> []
    defaultVideoSpeedControl =
      container Box []
        [ BoxChild defaultBoxChildProperties { expand  = False
                                            , fill    = False
                                            , padding = 5
                                            }
            (widget Label [#label := "Video Speed", #halign := AlignStart])
        , BoxChild defaultBoxChildProperties { expand  = False
                                            , fill    = False
                                            , padding = 5
                                            }
          (videoSpeedControl setDefaultVideoSpeed <&> ImportDefaultVideoSpeedChanged)
        ]
    classifyCheckBox =
      BoxChild defaultBoxChildProperties { expand = False, fill = False, padding = 10 } $
        widget
          CheckButton
          [ #label := "Classify parts automatically"
          , #active := classifyValue
          , #sensitive := classifyAvailable
          , onM #toggled (fmap ImportClassifySet . toggleButtonGetActive)
          ]
