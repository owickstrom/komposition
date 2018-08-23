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
                                        PolicyType (..), ScrolledWindow (..))
import           GI.Gtk.Declarative

import           FastCut.Library
import           FastCut.UserInterface

renderAsset :: Int -> Asset mt -> Int -> Widget (Event LibraryMode)
renderAsset focusedIdx asset' idx =
  node
    Label
    [ #label := toS (asset' ^. assetMetadata . path) <> " (" <>
      show (asset' ^. assetMetadata . duration) <>
      ")"
    , classes ["clip", focusedClass]
    ]
  where
    focusedClass :: Text
    focusedClass =
      if focusedIdx == idx
        then "focused"
        else "blurred"

libraryView :: [Asset mt] -> Int -> Widget (Event LibraryMode)
libraryView assets focusedIdx =
  container
    ScrolledWindow
    [ #hscrollbarPolicy := PolicyTypeNever
    , #vscrollbarPolicy := PolicyTypeAutomatic
    ]
    scrolledArea
  where
    scrolledArea :: Widget (Event LibraryMode)
    scrolledArea = container
       Box
       [#orientation := OrientationVertical, classes ["library"]] $ do
        boxChild
            False
            False
            0
            (node Label [#label := "Library", classes ["heading"]])
        boxChild True True 0 assetList
    assetList
      | null assets =
        node Label [#label := "You have no assets imported."]
      | otherwise =
         container
           Box
           [#orientation := OrientationVertical, classes ["clips"]] $
             for_ (zip assets [0..]) $ \(asset, i) ->
               boxChild False False 0 $ renderAsset focusedIdx asset i
