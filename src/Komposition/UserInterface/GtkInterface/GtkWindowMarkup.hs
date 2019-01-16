{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs            #-}
module Komposition.UserInterface.GtkInterface.GtkWindowMarkup where

import qualified GI.Gtk             as Gtk
import qualified GI.Gtk.Declarative as Declarative

data GtkWindowMarkup event where
   GtkWindowMarkup
    :: Declarative.Bin Gtk.Window event
    -> GtkWindowMarkup event

unGtkWindowMarkup
  :: GtkWindowMarkup event
  -> Declarative.Bin Gtk.Window event
unGtkWindowMarkup (GtkWindowMarkup decl) = decl
