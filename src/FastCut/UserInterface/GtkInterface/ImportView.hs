{-# OPTIONS_GHC -fno-warn-unticked-promoted-constructors #-}
{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE OverloadedLabels  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

-- | The import view of FastCut's GTK interface.
module FastCut.UserInterface.GtkInterface.ImportView
  ( importView
  ) where

import           FastCut.Prelude       hiding (State, on)

import           GI.Gtk                (Box (..), Button (..), CheckButton (..),
                                        Dialog (..), FileChooserButton (..),
                                        Label (..), Orientation (..),
                                        fileChooserGetFilename,
                                        toggleButtonGetActive)
import           GI.Gtk.Declarative    as Gtk

import           FastCut.UserInterface

importView :: ImportFileModel -> Widget (Event ImportMode)
importView ImportFileModel {..} =
  bin
    Dialog
    [ #title := "Import File"
    , on #destroy (CommandKeyMappedEvent Cancel)
    , #defaultWidth := 300
    , #defaultHeight := 400
    ] $
  container Box [classes ["import-view"], #orientation := OrientationVertical] $ do
    boxChild False False 10 $
      widget
        FileChooserButton
        [ onM
            #selectionChanged
            (fmap ImportFileSelected . fileChooserGetFilename)
        ]
    boxChild False False 10 $
      widget
        CheckButton
        [ #label := "Automatically split"
        , #active := autoSplitValue
        , #sensitive := autoSplitAvailable
        , onM #toggled (fmap ImportAutoSplitSet . toggleButtonGetActive)
        ]
    boxChild True False 10 $
      container Box [#orientation := OrientationHorizontal, #spacing := 10] $ do
        boxChild False False 0 $
          widget Button [#label := "Cancel", on #clicked ImportClicked]
        boxChild False False 0 $
          widget Button [#label := "Import", on #clicked ImportClicked]
