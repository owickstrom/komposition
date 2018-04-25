{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE RecordWildCards       #-}
module FastCut.VirtualWidget where

import           Control.Monad (forM_)
import           Data.HashSet  (HashSet)
import qualified Data.HashSet  as HashSet
import           Data.Maybe    (fromMaybe)
import           Data.Text     (Text)
import qualified GI.Gtk        as Gtk

-- * Rendering

{-data UpdateResult = Added | Removed | Modified | Unmodified-}

data Orientation = Vertical | Horizontal
  deriving (Eq, Show)

type ClassSet = HashSet Text

data Size = Size { width :: Int, height :: Int }
  deriving (Eq, Show)

data LabelProps = LabelProps { text :: Maybe Text, labelClasses :: ClassSet }
  deriving (Eq, Show)

labelProps :: LabelProps
labelProps = LabelProps {text = Nothing, labelClasses = mempty}

data BoxProps = BoxProps { orientation :: Orientation, boxClasses :: ClassSet, size :: Maybe Size }
  deriving (Eq, Show)

boxProps :: BoxProps
boxProps =
  BoxProps {orientation = Horizontal, boxClasses = mempty, size = Nothing}

data Element
  = Label LabelProps
  | Box BoxProps [Element]
  deriving (Eq, Show)

data GtkWidget where
  GtkWidget :: Gtk.IsWidget w => w -> GtkWidget

withGtkWidget :: GtkWidget -> (forall w . Gtk.IsWidget w => w -> r) -> r
withGtkWidget (GtkWidget w) f = f w

unsafeCastTo :: Gtk.GObject a => GtkWidget -> (Gtk.ManagedPtr a -> a) -> IO a
unsafeCastTo (GtkWidget obj) = flip Gtk.unsafeCastTo obj

addClasses :: Gtk.IsWidget w => w -> ClassSet -> IO ()
addClasses widget classes = do
  sc <- Gtk.widgetGetStyleContext widget
  mapM_ (Gtk.styleContextAddClass sc) classes

replaceClasses :: Gtk.IsWidget w => w -> ClassSet -> ClassSet -> IO ()
replaceClasses widget old new = do
  sc <- Gtk.widgetGetStyleContext widget
  mapM_ (Gtk.styleContextRemoveClass sc) (HashSet.difference old new)
  mapM_ (Gtk.styleContextAddClass sc)    (HashSet.difference new old)

setSize :: Gtk.IsWidget w => w -> Maybe Size -> IO ()
setSize widget = \case
  Just Size {..} ->
    Gtk.widgetSetSizeRequest widget (fromIntegral width) (fromIntegral height)
  Nothing -> return ()

render :: Element -> IO GtkWidget
render = \case
  Label LabelProps {..} -> do
    label <- Gtk.labelNew text
    label `addClasses` labelClasses
    return (GtkWidget label)
  Box BoxProps {..} children -> do
    box <- case orientation of
      Horizontal -> Gtk.boxNew Gtk.OrientationHorizontal 0
      Vertical   -> Gtk.boxNew Gtk.OrientationVertical 0
    box `setSize` size
    box `addClasses` boxClasses
    forM_ children $ \child -> do
      childWidget <- render child
      withGtkWidget childWidget (\w -> Gtk.boxPackStart box w True True 0)
    return (GtkWidget box)

update :: GtkWidget -> Element -> Element -> IO ()
update widget = curry $ \case
  (old           , new           ) | old == new -> return ()
  (Label oldProps, Label newProps)              -> do
    label <- widget `unsafeCastTo` Gtk.Label
    Gtk.labelSetLabel label (fromMaybe mempty (text newProps))
    replaceClasses label (labelClasses oldProps) (labelClasses newProps)
  (Box oldProps oldChildren, Box newProps newChildren) -> do
    box <- widget `unsafeCastTo` Gtk.Box
    box `setSize` size newProps
    replaceClasses box (boxClasses oldProps) (boxClasses newProps)
    updateAll      box oldChildren           newChildren
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
