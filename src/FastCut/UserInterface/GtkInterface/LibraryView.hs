{-# OPTIONS_GHC -fno-warn-unticked-promoted-constructors #-}
{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE OverloadedLabels  #-}
{-# LANGUAGE OverloadedStrings #-}

-- | The library view of FastCut's GTK interface.
module FastCut.UserInterface.GtkInterface.LibraryView
  ( libraryView
  ) where

import           FastCut.Prelude                         hiding (State, on)

import           Control.Lens
import           Data.Text                               (Text)
import           GI.Gtk.Declarative                      as Gtk

import           FastCut.Library
import           FastCut.UserInterface
import           FastCut.UserInterface.GtkInterface.View

renderAsset :: Int -> Asset mt -> Int -> BoxChild
renderAsset focusedIdx asset' idx =
  BoxChild False False 0 $
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

libraryView :: [Asset mt] -> Int -> IO (View LibraryMode)
libraryView assets focusedIdx =
  viewWithEvents $ \_ ->
  container
    ScrolledWindow
    [ #hscrollbarPolicy := PolicyTypeNever
    , #vscrollbarPolicy := PolicyTypeAutomatic
    ]
    (container
       Box
       [#orientation := OrientationVertical, classes ["library"]]
       [ BoxChild
           False
           False
           0
           (node Label [#label := "Library", classes ["heading"]])
       , BoxChild True True 0 assetList
       ])
  where
    assetList
      | null assets =
        node Label [#label := "You have no assets imported."]
      | otherwise =
         container
           Box
           [#orientation := OrientationVertical, classes ["clips"]]
           (zipWith (renderAsset focusedIdx) assets [0..])
