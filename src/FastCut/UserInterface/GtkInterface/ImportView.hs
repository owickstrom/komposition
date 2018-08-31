{-# OPTIONS_GHC -fno-warn-unticked-promoted-constructors #-}
{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE OverloadedLabels  #-}
{-# LANGUAGE OverloadedStrings #-}

-- | The import view of FastCut's GTK interface.
module FastCut.UserInterface.GtkInterface.ImportView
  ( importView
  ) where

import           FastCut.Prelude       hiding (State, on)

import           GI.Gtk                (Box (..), Button (..), CheckButton (..),
                                        FileChooserButton (..), Label (..),
                                        Orientation (..),
                                        fileChooserGetFilename,
                                        toggleButtonGetActive)
import           GI.Gtk.Declarative    as Gtk

import           FastCut.UserInterface

importView :: Widget (Event ImportMode)
importView =
  container Box [ classes ["import-view"], #orientation := OrientationVertical ] $ do
    boxChild True True 0 $ widget Label [#label := "Import Asset"]
    boxChild False False 0 $
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
        , onM
            #toggled
            (fmap ImportAutoSplitSet . toggleButtonGetActive)
        ]
    boxChild False False 10 $
      widget
        Button
        [ #label := "Import"
        , on #clicked ImportClicked
        ]
