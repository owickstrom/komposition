{-# LANGUAGE DuplicateRecordFields  #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs                  #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE UndecidableInstances   #-}
module FastCut.VirtualWidget where

import           Control.Monad (forM_)
import           Data.HashSet  (HashSet)
import qualified Data.HashSet  as HashSet
import           Data.Proxy    (Proxy (..))
import           Data.Text     (Text)
import qualified GI.Gtk        as Gtk

-- * Rendering

{-data UpdateResult = Added | Removed | Modified | Unmodified-}

class HasWidgetConstructor w where
  widgetConstructor :: Gtk.ManagedPtr w -> w

instance HasWidgetConstructor Gtk.Label where
  widgetConstructor = Gtk.Label

instance HasWidgetConstructor Gtk.Box where
  widgetConstructor = Gtk.Box

class Gtk.IsWidget w => VirtualWidget a w | a -> w where
  render :: a -> IO w
  update :: a -> a -> w -> IO ()

unsafeCastChildren
  :: (HasWidgetConstructor w, Gtk.IsContainer c, VirtualWidget a w) => c -> Proxy a -> IO [w]
unsafeCastChildren container _ =
  mapM (Gtk.unsafeCastTo widgetConstructor) =<< Gtk.containerGetChildren container

updateAll
  :: (HasWidgetConstructor w, Gtk.IsContainer c, VirtualWidget a w)
  => c
  -> Proxy a
  -> [a]
  -> [a]
  -> IO ()
updateAll container p os' ns' = do
  cs <- unsafeCastChildren container p
  sequence_ (go [] cs os' ns')
 where
  go :: (VirtualWidget a w) => [IO ()] -> [w] -> [a] -> [a] -> [IO ()]
  -- In case we have a corresponding old and new virtual widget, we update the
  -- GTK widget.
  go actions (w:ws) (o:os) (n:ns) = go (actions ++ [update o n w]) ws os ns
  -- When there are new virtual widgets, render and add them.
  go actions [] [] (n:ns) =
    let action = Gtk.containerAdd container =<< render n
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

-- * Elements

data Orientation = Vertical | Horizontal
  deriving (Eq, Show)

data Label = Label { text :: Text, classes :: HashSet Text }
  deriving (Eq, Show)

instance VirtualWidget Label Gtk.Label where
  render label = do
    widget <- Gtk.labelNew (Just (text label))
    sc <- Gtk.widgetGetStyleContext widget
    mapM_ (Gtk.styleContextAddClass sc) (classes label)
    return widget
  update old new widget
    | old == new = return ()
    | otherwise = do
      Gtk.labelSetLabel widget (text new)
      sc <- Gtk.widgetGetStyleContext widget
      mapM_ (Gtk.styleContextRemoveClass sc) (HashSet.difference (classes old) (classes new))
      mapM_ (Gtk.styleContextAddClass sc) (HashSet.difference (classes new) (classes old))

data Box a = Box { orientation :: Orientation, children :: [a] }
  deriving (Eq, Show)

instance (HasWidgetConstructor w, VirtualWidget a w) => VirtualWidget (Box a) Gtk.Box where
  render box = do
    widget <- case orientation box of
                Horizontal -> Gtk.boxNew Gtk.OrientationHorizontal 0
                Vertical   -> Gtk.boxNew Gtk.OrientationVertical 0
    forM_ (children box) $ \child -> do
      childWidget <- render child
      Gtk.boxPackEnd widget childWidget False False 0
    return widget
  update old new widget =
    updateAll widget (Proxy :: Proxy a) (children old) (children new)
