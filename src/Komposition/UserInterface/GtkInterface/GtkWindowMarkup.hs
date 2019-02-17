{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs            #-}
{-# LANGUAGE TypeFamilies     #-}
module Komposition.UserInterface.GtkInterface.GtkWindowMarkup where

import qualified GI.Gtk                         as Gtk
import qualified GI.Gtk.Declarative             as Declarative
import qualified GI.Gtk.Declarative.EventSource as Declarative
import           Komposition.UserInterface

data GtkWindowMarkup window event where
  GtkTopWindowMarkup
    :: Declarative.Bin (GtkWindowType 'TopWindow) event
    -> GtkWindowMarkup 'TopWindow event
  GtkModalMarkup
    :: Declarative.Bin (GtkWindowType 'Modal) event
    -> GtkWindowMarkup 'Modal event

type family GtkWindowType window where
  GtkWindowType 'TopWindow = Gtk.Window
  GtkWindowType 'Modal = Gtk.Dialog

unGtkWindowMarkup
  :: GtkWindowMarkup window event
  -> Declarative.Bin (GtkWindowType window) event
unGtkWindowMarkup (GtkTopWindowMarkup decl) = decl
unGtkWindowMarkup (GtkModalMarkup decl)     = decl

instance Declarative.Patchable (GtkWindowMarkup window) where
  create (GtkTopWindowMarkup decl) = Declarative.create decl
  create (GtkModalMarkup     decl) = Declarative.create decl
  patch s (GtkTopWindowMarkup d1) (GtkTopWindowMarkup d2) =
    Declarative.patch s d1 d2
  patch s (GtkModalMarkup d1) (GtkModalMarkup d2) = Declarative.patch s d1 d2

instance Declarative.EventSource (GtkWindowMarkup window) where
  subscribe (GtkTopWindowMarkup decl) = Declarative.subscribe decl
  subscribe (GtkModalMarkup decl)     = Declarative.subscribe decl
