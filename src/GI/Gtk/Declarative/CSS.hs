-- | Functions for modifying CSS classes in 'Gtk.StyleContext'
-- objects.

module GI.Gtk.Declarative.CSS where

import           Data.HashSet (HashSet)
import qualified Data.HashSet as HashSet
import           Data.Text    (Text)
import qualified GI.Gtk       as Gtk

type ClassSet = HashSet Text

addClasses :: Gtk.IsWidget w => w -> ClassSet -> IO ()
addClasses widget cs = do
  sc <- Gtk.widgetGetStyleContext widget
  mapM_ (Gtk.styleContextAddClass sc) cs

replaceClasses :: Gtk.IsWidget w => w -> ClassSet -> ClassSet -> IO ()
replaceClasses widget old new = do
  sc <- Gtk.widgetGetStyleContext widget
  mapM_ (Gtk.styleContextRemoveClass sc) (HashSet.difference old new)
  mapM_ (Gtk.styleContextAddClass sc)    (HashSet.difference new old)
