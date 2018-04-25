-- | The "functional user interface" module (FUI) represents a hierarchy of
-- immutable user interface objects, usually constructed in a pure setting, and
-- provides a patching mechanism for performing minimal updates using the
-- underlying imperative GTK library.

{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE RecordWildCards       #-}
module FastCut.FUI where

import           Control.Monad (forM_)
import           Data.HashSet  (HashSet)
import qualified Data.HashSet  as HashSet
import           Data.Maybe    (fromMaybe)
import           Data.Text     (Text)
import           Data.GI.Base.CallStack  (HasCallStack)
import qualified GI.Gtk        as Gtk

-- * Rendering

data Orientation = Vertical | Horizontal
  deriving (Eq, Show)

type ClassSet = HashSet Text

data Size = Size { width :: Int, height :: Int }
  deriving (Eq, Show)

data LabelProps = LabelProps { text :: Maybe Text, labelClasses :: ClassSet }
  deriving (Eq, Show)

labelProps :: LabelProps
labelProps = LabelProps {text = Nothing, labelClasses = mempty}

data BoxChildProps = BoxChildProps { expand :: Bool, fill :: Bool, padding :: Word }
  deriving (Eq, Show)

boxChildProps :: BoxChildProps
boxChildProps = BoxChildProps {expand = False, fill = False, padding = 0}

data BoxChild = BoxChild BoxChildProps Object
  deriving (Eq, Show)

boxChildElement :: BoxChild -> Object
boxChildElement (BoxChild _ obj) = obj

data BoxProps = BoxProps { orientation :: Orientation, boxClasses :: ClassSet, size :: Maybe Size }
  deriving (Eq, Show)

boxProps :: BoxProps
boxProps =
  BoxProps {orientation = Horizontal, boxClasses = mempty, size = Nothing}

data Object
  = Label LabelProps
  | Box BoxProps [BoxChild]
  | ScrollArea Object
  deriving (Eq, Show)

data GtkWidget where
  GtkWidget :: Gtk.IsWidget w => w -> GtkWidget

withGtkWidget :: GtkWidget -> (forall w . Gtk.IsWidget w => w -> r) -> r
withGtkWidget (GtkWidget w) f = f w

unsafeCastTo
  :: (HasCallStack, Gtk.GObject a)
  => GtkWidget
  -> (Gtk.ManagedPtr a -> a)
  -> IO a
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

create :: Object -> IO GtkWidget
create = \case
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
    forM_ children $ \(BoxChild BoxChildProps {..} child) -> do
      childWidget <- create child
      withGtkWidget
        childWidget
        (\w -> Gtk.boxPackStart box w expand fill (fromIntegral padding))
    return (GtkWidget box)
  ScrollArea child -> do
    scrollArea <- Gtk.scrolledWindowNew Gtk.noAdjustment Gtk.noAdjustment
    Gtk.scrolledWindowSetPolicy scrollArea
                                Gtk.PolicyTypeAutomatic
                                Gtk.PolicyTypeNever
    childWidget <- create child
    withGtkWidget childWidget (Gtk.containerAdd scrollArea)
    return (GtkWidget scrollArea)

patch :: GtkWidget -> Object -> Object -> IO ()
patch widget = curry $ \case
  (old           , new           ) | old == new -> return ()
  (Label oldProps, Label newProps)              -> do
    label <- widget `unsafeCastTo` Gtk.Label
    Gtk.labelSetLabel label (fromMaybe mempty (text newProps))
    replaceClasses label (labelClasses oldProps) (labelClasses newProps)
  (Box oldProps oldChildren, Box newProps newChildren) -> do
    box <- widget `unsafeCastTo` Gtk.Box
    box `setSize` size newProps
    replaceClasses box (boxClasses oldProps) (boxClasses newProps)
    patchAll box
             (map boxChildElement oldChildren)
             (map boxChildElement newChildren)
  (ScrollArea oldChild, ScrollArea newChild) -> do
    box      <- widget `unsafeCastTo` Gtk.ScrolledWindow
    viewport <-
      Gtk.unsafeCastTo Gtk.Viewport
      =<< requireSingle
      =<< Gtk.containerGetChildren box
    childWidget <- requireSingle =<< Gtk.containerGetChildren viewport
    patch (GtkWidget childWidget) oldChild newChild
   where
    requireSingle [w] = return w
    requireSingle _   = fail "Expected a single child widget."
  (old, new) ->
    fail ("What to do with " ++ show old ++ " and " ++ show new ++ "?")

patchAll :: (Gtk.IsContainer c) => c -> [Object] -> [Object] -> IO ()
patchAll container os' ns' = do
  cs <- Gtk.containerGetChildren container
  sequence_ (go [] cs os' ns')
 where
  -- In case we have a corresponding old and new virtual widget, we patch the
  -- GTK widget.
  go actions (w:ws) (o:os) (n:ns) =
    let action = patch (GtkWidget w) o n in go (actions ++ [action]) ws os ns
  -- When there are new virtual widgets, create and add them.
  go actions [] [] (n:ns) =
    let action = do
          widget <- create n
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
