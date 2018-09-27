{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE OverloadedLabels  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

-- | The import view of Komposition's GTK interface.
module Komposition.UserInterface.GtkInterface.ImportView
  ( importView
  ) where

import           Komposition.Prelude       hiding (State, on)

import           GI.Gtk                (Box (..), Button (..), CheckButton (..),
                                        FileChooserButton (..),
                                        Orientation (..), Window (..),
                                        fileChooserGetFilename,
                                        toggleButtonGetActive)
import           GI.Gtk.Declarative    as Gtk

import           Komposition.UserInterface

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
