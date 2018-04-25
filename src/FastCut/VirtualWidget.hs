{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UndecidableInstances  #-}
module FastCut.VirtualWidget where

import           Control.Monad (forM_)
import           Data.HashSet  (HashSet)
import qualified Data.HashSet  as HashSet
import           Data.Text     (Text)
import qualified GI.Gtk        as Gtk

-- * Rendering

{-data UpdateResult = Added | Removed | Modified | Unmodified-}

data Orientation = Vertical | Horizontal
  deriving (Eq, Show)

type ClassSet = HashSet Text

data Element
  = Label { text :: Text, classes :: ClassSet }
  | Box { orientation :: Orientation, classes :: ClassSet, children :: [Element] }
  deriving (Eq, Show)

data GtkWidget where
  GtkWidget :: Gtk.IsWidget w => w -> GtkWidget

withGtkWidget :: GtkWidget -> (forall w . Gtk.IsWidget w => w -> r) -> r
withGtkWidget (GtkWidget w) f = f w

unsafeCastTo :: Gtk.GObject a => GtkWidget -> (Gtk.ManagedPtr a -> a) -> IO a
unsafeCastTo (GtkWidget obj) = flip Gtk.unsafeCastTo obj

addClasses :: Gtk.IsWidget w => w -> ClassSet -> IO ()
addClasses widget classes = do
  sc    <- Gtk.widgetGetStyleContext widget
  mapM_ (Gtk.styleContextAddClass sc) classes

replaceClasses :: Gtk.IsWidget w => w -> ClassSet -> ClassSet -> IO ()
replaceClasses widget old new = do
  sc <- Gtk.widgetGetStyleContext widget
  mapM_ (Gtk.styleContextRemoveClass sc)
        (HashSet.difference old new)
  mapM_ (Gtk.styleContextAddClass sc)
        (HashSet.difference new old)

render :: Element -> IO GtkWidget
render = \case
  Label {..} -> do
    label <- Gtk.labelNew (Just text)
    label `addClasses` classes
    return (GtkWidget label)
  Box {..} -> do
    box <- case orientation of
      Horizontal -> Gtk.boxNew Gtk.OrientationHorizontal 0
      Vertical   -> Gtk.boxNew Gtk.OrientationVertical 0
    box `addClasses` classes
    forM_ children $ \child -> do
      childWidget <- render child
      withGtkWidget childWidget (\w -> Gtk.boxPackStart box w True True 0)
    return (GtkWidget box)

update :: GtkWidget -> Element -> Element -> IO ()
update widget = curry $ \case
  (old        , new        ) | old == new -> return ()
  (old@Label{}, new@Label{})              -> do
    label <- widget `unsafeCastTo` Gtk.Label
    Gtk.labelSetLabel label (text new)
    replaceClasses label (classes old) (classes new)
  (old@Box{}, new@Box{}) -> do
    box <- widget `unsafeCastTo` Gtk.Box
    replaceClasses box (classes old) (classes new)
    updateAll box (children old) (children new)
  (old, new) ->
    fail ("What to do with " ++ show old ++ " and " ++ show new ++ "?")

updateAll :: (Gtk.IsContainer c) => c -> [Element] -> [Element] -> IO ()
updateAll container os' ns' = do
  cs <- Gtk.containerGetChildren container
  sequence_ (go [] cs os' ns')
 where
  -- In case we have a corresponding old and new virtual widget, we update the
  -- GTK widget.
  go actions (w:ws) (o:os) (n:ns) =
    let action = update (GtkWidget w) o n in go (actions ++ [action]) ws os ns
  -- When there are new virtual widgets, render and add them.
  go actions [] [] (n:ns) =
    let action = do
          widget <- render n
          withGtkWidget widget (Gtk.containerAdd container)
    in  go (actions ++ [action]) [] [] ns
  -- When a virtual widget has been removed, remove the GTK widget from the
  -- container.
  go actions (w:ws) (_:os) [] =
    let action = Gtk.containerRemove container w
    in  go (actions ++ [action]) ws os []
  -- When there are more old virtual widgets than GTK widgets, we can safely
  -- drop the virtual widgets and go on.
  go actions [] (_:_) ns = go actions [] [] ns
  -- But, when there are stray GTK widgets without corresponding virtual
  -- widgets, something has gone terribly wrong, and we clean that mess up by
  -- removing the GTK widget.
  go actions (w:ws) [] ns =
    let action = Gtk.containerRemove container w
    in  go (actions ++ [action]) ws [] ns
  -- Lastly, when we have gone through all widgets, we return the actions.
  go actions [] [] [] = actions
