{-# OPTIONS_GHC -fno-warn-unticked-promoted-constructors #-}
{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedLabels  #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Requires a confirmation before exiting.
module FastCut.UserInterface.GtkInterface.ExitingView
  ( exitingView
  ) where

import           FastCut.Prelude                         hiding (State, on)

import           GI.Gtk.Declarative                      as Gtk

import           FastCut.UserInterface
import           FastCut.UserInterface.GtkInterface.View

exitingView :: IO (View ExitingMode)
exitingView =
  viewWithEvents $ \_ ->
    container
      Box
      [#orientation := OrientationVertical, classes ["exiting-view"]]
      [ BoxChild
          False
          False
          0
          (node Label [#label := "Do you really want to exit? (y/n)"])
      ]
