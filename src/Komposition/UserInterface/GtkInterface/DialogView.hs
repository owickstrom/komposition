{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE OverloadedLabels  #-}
{-# LANGUAGE OverloadedStrings #-}
module Komposition.UserInterface.GtkInterface.DialogView where

import           Komposition.Prelude     hiding ( on )

import           GI.Gtk                         ( Label(..)
                                                , Align(..)
                                                , Window(..)
                                                , Box(..)
                                                , Button(..)
                                                , Orientation(..)
                                                )
import           GI.Gtk.Declarative
import           Komposition.UserInterface.Dialog
import           Komposition.UserInterface.GtkInterface.GtkWindowMarkup

instance DialogView GtkWindowMarkup where
  dialogView props =
    GtkWindowMarkup $
      bin Window [ #title := dialogTitle props
                 , on #deleteEvent (const (True, DialogClosed))
                 ] $
        container Box [ #orientation := OrientationVertical, classes ["dialog"] ] $ do
          boxChild False False 0 $
            widget Label [#label := dialogMessage props]
          boxChild False False 0 $
            container Box [classes ["choices"], #halign := AlignEnd] $
              forM_ (dialogChoices props) $ \c ->
                boxChild False False 0 $
                  widget Button [ #label := toButtonLabel c
                                , classes ["choice"]
                                , on #clicked (DialogChoiceSelected c)
                                ]
