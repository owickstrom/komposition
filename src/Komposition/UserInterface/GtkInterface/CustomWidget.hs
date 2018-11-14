{-# LANGUAGE DeriveFunctor    #-}
{-# LANGUAGE OverloadedLabels #-}
module Komposition.UserInterface.GtkInterface.CustomWidget where

import           Komposition.Prelude

import qualified GI.Gtk                         as Gtk
import           GI.Gtk.Declarative
import           GI.Gtk.Declarative.EventSource
import           GI.Gtk.Declarative.State

data CustomPatch widget customData
  = CustomReplace
  | CustomModify (widget -> IO SomeState)
  | CustomKeep

data CustomWidget widget customData event =
  CustomWidget
  { customWidget :: Gtk.ManagedPtr widget -> widget
  , customCreate :: customData -> IO SomeState
  , customPatch :: SomeState -> customData -> customData -> CustomPatch widget customData
  , customSubscribe :: customData -> widget -> (event -> IO ()) -> IO Subscription
  , customData :: customData
  } deriving (Functor)

instance Gtk.IsWidget widget => Patchable (CustomWidget widget customData) where
  create custom = customCreate custom (customData custom)
  patch state' old new =
    case customPatch old state' (customData old) (customData new) of
      CustomReplace -> Replace (customCreate new (customData new))
      CustomModify f -> Modify (f =<< Gtk.unsafeCastTo (customWidget new) =<< someStateWidget state')
      CustomKeep -> Keep

instance Gtk.GObject widget => EventSource (CustomWidget widget customData) where
  subscribe custom state' cb = do
    w' <- Gtk.unsafeCastTo (customWidget custom) =<< someStateWidget state'
    customSubscribe custom (customData custom) w' cb
