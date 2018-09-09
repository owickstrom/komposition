{-# OPTIONS_GHC -fno-warn-unticked-promoted-constructors #-}
{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE OverloadedLabels  #-}
{-# LANGUAGE OverloadedStrings #-}

-- | The library view of FastCut's GTK interface.
module FastCut.UserInterface.GtkInterface.LibraryView
  ( libraryView
  ) where

import           FastCut.Prelude       hiding (State, on)

import           Control.Lens
import           Data.Text             (Text)
import           GI.Gtk                (Box (..), Label (..), Orientation (..),
                                        PolicyType (..), ScrolledWindow (..),
                                        Window (..))
import           GI.Gtk.Declarative

import           FastCut.Library
import           FastCut.MediaType
import           FastCut.UserInterface

renderAsset :: Int -> Asset mt -> Int -> Widget (Event LibraryMode)
renderAsset focusedIdx asset' idx =
  widget Label [#label := label, classes ["clip", focusedClass]]
  where
    focusedClass :: Text
    focusedClass =
      if focusedIdx == idx
        then "focused"
        else "blurred"
    label :: Text
    label =
      toS $ (asset' ^. assetMetadata . path) <> " (" <>
      show (asset' ^. assetMetadata . duration) <>
      ")"

libraryView :: SMediaType mt -> [Asset mt] -> Int -> Widget (Event LibraryMode)
libraryView mediaType assets focusedIdx =
  bin
    Window
    [ #title := "Library"
    , on #destroy (CommandKeyMappedEvent Cancel)
    , #defaultWidth := 300
    , #defaultHeight := 400
    ] $
  bin
    ScrolledWindow
    [ #hscrollbarPolicy := PolicyTypeNever
    , #vscrollbarPolicy := PolicyTypeAutomatic
    ]
    assetList
  where
    assetList
      | null assets = widget Label [#label := noAssetsMessage]
      | otherwise =
        container Box [#orientation := OrientationVertical, classes ["clips"]] $
        for_ (zip assets [0 ..]) $ \(asset, i) ->
          boxChild False False 0 $ renderAsset focusedIdx asset i
    noAssetsMessage =
      case mediaType of
        SVideo -> "You have no video assets imported into your library."
        SAudio -> "You have no audio assets imported into your library."
