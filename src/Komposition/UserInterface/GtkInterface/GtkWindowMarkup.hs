{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs            #-}
module Komposition.UserInterface.GtkInterface.GtkWindowMarkup where

import qualified GI.Gtk                 as Gtk
import qualified GI.Gtk.Declarative     as Declarative
import qualified GI.Gtk.Declarative.Bin as Declarative

data GtkWindowMarkup event where
   GtkWindowMarkup
    :: Declarative.BinChild Gtk.Window Declarative.Widget
    => Declarative.Bin Gtk.Window Declarative.Widget event
    -> GtkWindowMarkup event

unGtkWindowMarkup
  :: GtkWindowMarkup event
  -> Declarative.Bin Gtk.Window Declarative.Widget event
unGtkWindowMarkup (GtkWindowMarkup decl) = decl
