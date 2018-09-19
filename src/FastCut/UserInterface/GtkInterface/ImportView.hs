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
                                        FileChooserButton (..),
                                        Orientation (..), Window (..),
                                        fileChooserGetFilename,
                                        toggleButtonGetActive)
import           GI.Gtk.Declarative    as Gtk

import           FastCut.UserInterface

importView :: ImportFileModel -> Widget (Event ImportMode)
importView ImportFileModel {..} =
  bin
    Window
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
    boxChild False False 10 $
      widget Button [#label := "Import", on #clicked ImportClicked]
