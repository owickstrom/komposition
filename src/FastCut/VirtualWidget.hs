{-# LANGUAGE DuplicateRecordFields  #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE UndecidableInstances   #-}
module FastCut.VirtualWidget where

import qualified GI.Gtk as Gtk

data Label m = Label { model :: m, widget :: Gtk.Label }

data Container m = Container { models :: [m], widget :: Gtk.Container }

data Result = Created | Removed | Modified | Unmodified deriving (Eq)

class VirtualWidget a where
  reconcileWith :: a -> a -> IO Result

class HasWidget a w | a -> w where
  getWidget :: a -> w

instance HasWidget (Label m) Gtk.Label where
  getWidget = widget

instance Eq m => VirtualWidget (Label m) where
  reconcileWith (Label oldModel oldWidget) (Label newModel newWidget)
    | oldModel == newModel = return Unmodified
    | otherwise = do
      Gtk.labelSetLabel oldWidget =<< Gtk.labelGetLabel newWidget
      Gtk.labelSetAttributes oldWidget =<< Gtk.labelGetAttributes newWidget
      return Modified

instance HasWidget (Container m) Gtk.Container where
  getWidget = widget

reconcileAllWith
  :: (Gtk.IsWidget w, HasWidget m w, VirtualWidget m)
  => Gtk.Container
  -> [m]
  -> [m]
  -> IO [Result]
reconcileAllWith container = go []
 where
  go
    :: (Gtk.IsWidget w, HasWidget m w, VirtualWidget m)
    => [Result]
    -> [m]
    -> [m]
    -> IO [Result]
  go acc (o:os) (n:ns) = do
    r <- reconcileWith o n
    go (acc ++ [r]) os ns
  go acc [] (n:ns) = do
    Gtk.containerAdd container (getWidget n)
    go (acc ++ [Created]) [] ns
  go acc (o:os) [] = do
    Gtk.containerRemove container (getWidget o)
    go (acc ++ [Created]) os []
  go acc [] [] = return acc

instance (Gtk.IsWidget w, HasWidget m w, VirtualWidget m) => VirtualWidget (Container m) where
  reconcileWith old new = do
    results <- reconcileAllWith (getWidget old) (models old) (models new)
    if all (== Unmodified) results
       then return Unmodified
       else return Modified
