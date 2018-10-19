{-# LANGUAGE DeriveFunctor    #-}
{-# LANGUAGE OverloadedLabels #-}
module Komposition.UserInterface.GtkInterface.CustomWidget where

import           Komposition.Prelude

import qualified GI.Gtk                         as Gtk
import           GI.Gtk.Declarative
import           GI.Gtk.Declarative.EventSource

data CustomPatch widget customData
  = CustomReplace
  | CustomModify (widget -> IO ())
  | CustomKeep

data CustomWidget widget customData event =
  CustomWidget
  { customWidget :: Gtk.ManagedPtr widget -> widget
  , customCreate :: customData -> IO widget
  , customPatch :: customData -> customData -> CustomPatch widget customData
  , customSubscribe :: customData -> widget -> (event -> IO ()) -> IO Subscription
  , customData :: customData
  } deriving (Functor)

instance Gtk.IsWidget widget => Patchable (CustomWidget widget customData) where
  create custom = Gtk.toWidget =<< customCreate custom (customData custom)
  patch old new =
    case customPatch old (customData old) (customData new) of
      CustomReplace -> Replace (Gtk.toWidget =<< customCreate new (customData new))
      CustomModify f -> Modify (Gtk.unsafeCastTo (customWidget new) >=> f)
      CustomKeep -> Keep

instance Gtk.GObject widget => EventSource (CustomWidget widget customData) where
  subscribe custom w cb = do
    w' <- Gtk.unsafeCastTo (customWidget custom) w
    customSubscribe custom (customData custom) w' cb
