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
                                        FileChooserButton (..),
                                        Label(..),
                                        toggleButtonGetActive,
                                        fileChooserGetFilename)
import           GI.Gtk.Declarative    as Gtk

import           FastCut.UserInterface

importView :: Widget (Event ImportMode)
importView =
  container Box [ classes ["import-view"] ] $ do
    boxChild True True 0 $ node Label [#label := "Import Asset"]
    boxChild False False 0 $
      node
        FileChooserButton
        [ onM
          #selectionChanged
          (fmap ImportFileSelected . fileChooserGetFilename)
        ]
    boxChild False False 10 $
      node
        CheckButton
        [ #label := "Automatically split video"
        , onM
            #toggled
            (fmap ImportAutoSplitSet . toggleButtonGetActive)
        ]
    boxChild False False 10 $
      node
        Button
        [ #label := "Import"
        , on #clicked ImportClicked
        ]
