{-# OPTIONS_GHC -fno-warn-unticked-promoted-constructors #-}
{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE OverloadedLabels  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

-- | The library view of FastCut's GTK interface.
module FastCut.UserInterface.GtkInterface.LibraryView
  ( libraryView
  ) where

import           FastCut.Prelude       hiding (State, on)

import           Control.Lens
import           Data.Text             (Text)
import           GI.Gtk                (Box (..), Button (..), Label (..),
                                        ListBox (..), ListBoxRow (..),
                                        Orientation (..), PolicyType (..),
                                        ScrolledWindow (..), SelectionMode (..),
                                        Window (..))
import           GI.Gtk.Declarative

import           FastCut.Library
import           FastCut.MediaType
import           FastCut.UserInterface

renderAsset ::
  Asset mt
  -> MarkupOf (Bin ListBoxRow Widget) (Event LibraryMode) ()
renderAsset asset' =
  bin ListBoxRow [on #activate LibrarySelectionConfirmed] $
  widget Label [#label := label]
  where
    label :: Text
    label =
      toS $ (asset' ^. assetMetadata . path) <> " (" <>
      show (asset' ^. assetMetadata . duration) <>
      ")"

libraryView :: SMediaType mt -> SelectAssetsModel mt -> Widget (Event LibraryMode)
libraryView mediaType SelectAssetsModel {..} =
  bin
    Window
    [ #title := "Library"
    , on #destroy (CommandKeyMappedEvent Cancel)
    , #defaultWidth := 300
    , #defaultHeight := 400
    , classes ["library"]
    ] $
  bin
    ScrolledWindow
    [ #hscrollbarPolicy := PolicyTypeNever
    , #vscrollbarPolicy := PolicyTypeAutomatic
    ]
    assetSelectList
  where
    assetSelectList =
      container Box [#orientation := OrientationVertical] $ do
        boxChild True True 0 $
          container
            ListBox
            [ #selectionMode := SelectionModeMultiple
            , onM #selectedRowsChanged emitAssetsSelected
            , on #activateCursorRow LibrarySelectionConfirmed
            , classes ["clips"]
            ] $
          for_ allAssets renderAsset
        boxChild False False 10 $ container Box [] $ do
          boxChild True True 10 $
            widget
              Button
              [ #label := "Cancel"
              , on #clicked (CommandKeyMappedEvent Cancel)
              ]
          boxChild True True 10 $
            widget
              Button
              [ #label := "Select"
              , #sensitive := not (null selectedAssets)
              , on #clicked LibrarySelectionConfirmed
              ]
    emitAssetsSelected listBox = do
      is <- map fromIntegral <$> (#getSelectedRows listBox >>= mapM #getIndex)
      let selected = (allAssets ^.. traversed . indices (`elem` is))
      return (LibraryAssetsSelected mediaType selected)
