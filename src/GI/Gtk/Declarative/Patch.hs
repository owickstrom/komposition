-- | A 'Patch' represents a possible 'IO' action to apply to a
-- 'Gtk.Widget' to make it reflect the new declarative object.

module GI.Gtk.Declarative.Patch
  ( Patch(..)
  , Patchable(..)
  ) where

import qualified GI.Gtk as Gtk

data Patch
  = Modify (Gtk.Widget -> IO ())
  | Replace (IO Gtk.Widget)
  | Keep

class Patchable obj where
  create ::  obj -> IO Gtk.Widget
  patch :: obj -> obj -> Patch
