{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE OverloadedLabels  #-}
{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE OverloadedStrings #-}
module Komposition.UserInterface.GtkInterface.DialogView where

import qualified Data.Vector                                            as Vector
import           Komposition.Prelude                                    hiding
                                                                         (on)

import           GI.Gtk                                                 (Align (..),
                                                                         Box (..),
                                                                         Button (..),
                                                                         Dialog (..),
                                                                         Label (..),
                                                                         Orientation (..),
                                                                         Window (..))
import           GI.Gtk.Declarative
import           Komposition.UserInterface.Dialog
import           Komposition.UserInterface.GtkInterface.GtkWindowMarkup

instance DialogView GtkWindowMarkup where
  dialogView props =
    GtkModalMarkup $
      bin Window [ #title := dialogTitle props
                 , on #deleteEvent (const (True, DialogClosed))
                 ] $
        container Box [ #orientation := OrientationVertical, classes ["dialog"] ]
        [
          BoxChild defaultBoxChildProperties { expand = False, fill = False, padding = 0 } $
            widget Label [#label := dialogMessage props]
        , BoxChild defaultBoxChildProperties { expand = False, fill = False, padding = 0 } $
            container Box [classes ["choices"], #halign := AlignEnd] $
              Vector.fromList (dialogChoices props) <&> \c ->
                BoxChild defaultBoxChildProperties { expand = False, fill = False, padding = 0 } $
                  widget Button [ #label := toButtonLabel c
                                , classes ["choice"]
                                , on #clicked (DialogChoiceSelected c)
                                ]
        ]
