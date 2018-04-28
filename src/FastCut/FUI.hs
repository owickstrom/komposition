{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs                  #-}
{-# LANGUAGE KindSignatures         #-}
{-# LANGUAGE LambdaCase             #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE OverloadedLabels       #-}
{-# LANGUAGE RankNTypes             #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE StandaloneDeriving     #-}
{-# LANGUAGE TypeApplications       #-}
{-# LANGUAGE TypeOperators          #-}

-- | The "functional user interface" module (FUI) represents a hierarchy of
-- immutable user interface objects, usually constructed in a pure setting, and
-- provides a patching mechanism for performing minimal updates using the
-- underlying imperative GTK library.
module FastCut.FUI where

import           Control.Monad           (forM_)
import           Data.Either             (partitionEithers)
import           Data.Foldable           (fold)
import qualified Data.GI.Base            as GI
import qualified Data.GI.Base.Attributes as GI
import           Data.HashSet            (HashSet)
import qualified Data.HashSet            as HashSet
import           Data.Text               (Text)
import           Data.Typeable
import           GHC.TypeLits            (Symbol)
import qualified GI.Gtk                  as Gtk

class Patchable o where
  create ::  o -> IO Gtk.Widget
  patch :: Gtk.IsWidget w => w -> o -> o -> IO ()

data Object where
  Object :: (Eq o, Show o, Typeable o, Patchable o) => o -> Object

deriving instance Show Object

instance Eq Object where
  Object (a :: o) == Object (b :: n) =
    case eqT @o @n  of
      Just Refl -> a == b
      Nothing   -> False

-- * Rendering

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

patchAll :: Gtk.IsContainer c => c -> [Object] -> [Object] -> IO ()
patchAll container os' ns' = do
  cs <- Gtk.containerGetChildren container
  sequence_ (go [] cs os' ns')
 where
  -- In case we have a corresponding old and new virtual widget, we patch the
  -- GTK widget.
  go actions (w:ws) (Object (o :: t) : os) (Object (n :: t'):ns) =
    case eqT @t @t' of
      Just Refl ->
        let action = patch w o n in go (actions ++ [action]) ws os ns
      Nothing ->
        -- TODO: Replace
        let action = fail "Can't do replace yet." in go (actions ++ [action]) ws os ns
  -- When there are new virtual widgets, create and add them.
  go actions [] [] (Object n:ns) =
    let action = do
          widget <- create n
          Gtk.containerAdd container widget
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

-- -- * Generic GTK patchable object

data AttrPair obj where
  (:=)
    :: (GI.AttrGetC info obj attr value
      , GI.AttrOpAllowed 'GI.AttrConstruct info obj
      , GI.AttrOpAllowed 'GI.AttrSet info obj
      , GI.AttrSetTypeConstraint info value
      , Typeable attr
      , Eq value
      )
    =>  GI.AttrLabelProxy (attr :: Symbol) -> value -> AttrPair obj
  Classes
    :: Gtk.IsWidget obj
    => ClassSet
    -> AttrPair obj

classes :: Gtk.IsWidget obj => ClassSet -> AttrPair obj
classes = Classes

instance Eq (AttrPair obj) where
  ((_ :: GI.AttrLabelProxy attr1) := v1) == ((_ :: GI.AttrLabelProxy attr2) := v2) =
    case eqT @attr1 @attr2 of
      Just Refl -> v1 == v2
      Nothing   -> False
  Classes c1 == Classes c2 = c1 == c2
  _ == _ = False

data GtkLeaf a where
  GtkLeaf :: (Typeable a, Gtk.IsWidget a) => (Gtk.ManagedPtr a -> a) -> [AttrPair a] -> GtkLeaf a

instance Eq (GtkLeaf a) where
  GtkLeaf _ a1 == GtkLeaf _ a2 =
    a1 == a2

instance Show (GtkLeaf a) where
  show = \case
    GtkLeaf{} -> "GtkLeaf"

data GtkContainer a where
  GtkContainer :: (Typeable a, Gtk.IsWidget a, Gtk.IsContainer a) => (Gtk.ManagedPtr a -> a) -> [AttrPair a] -> [Object] -> GtkContainer a

instance Eq (GtkContainer a) where
  GtkContainer _ a1 c1 == GtkContainer _ a2 c2 =
    a1 == a2 && c1 == c2

instance Show (GtkContainer a) where
  show = \case
    GtkContainer{} -> "GtkContainer"

instance Patchable (GtkLeaf a) where
  create = \case
    (GtkLeaf ctor attrs) -> do
        let (attrOps, classSets) = partitionEithers (map toOpOrClass attrs)
        widget <- Gtk.new ctor attrOps
        addClasses widget (fold classSets)
        Gtk.toWidget widget
    where
      toOpOrClass :: AttrPair obj -> Either (GI.AttrOp obj 'GI.AttrConstruct) ClassSet
      toOpOrClass =
        \case
          (attr := value) -> Left (attr Gtk.:= value)
          Classes c -> Right c
  patch widget (GtkLeaf _ oldAttrs) (GtkLeaf ctor newAttrs) = do
    let (_, _oldClassSets) = partitionEithers (map toOpOrClass oldAttrs)
        (newAttrOps, newClassSets) = partitionEithers (map toOpOrClass newAttrs)
    w <- Gtk.unsafeCastTo ctor widget
    Gtk.set w newAttrOps
    addClasses w (fold newClassSets)
    where
      toOpOrClass :: AttrPair obj -> Either (GI.AttrOp obj 'GI.AttrSet) ClassSet
      toOpOrClass =
        \case
          (attr := value) -> Left (attr Gtk.:= value)
          Classes c -> Right c

instance Patchable (GtkContainer a) where
  create (GtkContainer ctor attrs children) = do
        let (attrOps, classSets) = partitionEithers (map toOpOrClass attrs)
        widget <- Gtk.new ctor attrOps
        addClasses widget (fold classSets)
        forM_ children $ \(Object child) ->
          create child >>= Gtk.containerAdd widget
        Gtk.toWidget widget
    where
      toOpOrClass :: AttrPair obj -> Either (GI.AttrOp obj 'GI.AttrConstruct) ClassSet
      toOpOrClass =
        \case
          (attr := value) -> Left (attr Gtk.:= value)
          Classes c -> Right c
  patch widget (GtkContainer _ _ oldChildren) (GtkContainer ctor newAttrs newChildren) = do
        let (_, oldClassSets) = partitionEithers (map toOpOrClass newAttrs)
            (newAttrOps, newClassSets) = partitionEithers (map toOpOrClass newAttrs)
        w <- Gtk.unsafeCastTo ctor widget
        Gtk.set w newAttrOps
        replaceClasses w (fold oldClassSets) (fold newClassSets)
        Gtk.castTo Gtk.Container widget >>= \case
          Just container -> patchAll container oldChildren newChildren
          Nothing        -> fail "Not a container."
    where
      toOpOrClass :: AttrPair obj -> Either (GI.AttrOp obj 'GI.AttrSet) ClassSet
      toOpOrClass =
        \case
          (attr := value) -> Left (attr Gtk.:= value)
          Classes c -> Right c

node :: (Typeable a, Gtk.IsWidget a) => (Gtk.ManagedPtr a -> a) -> [AttrPair a] -> Object
node ctor attrs = Object (GtkLeaf ctor attrs)

container :: (Typeable a, Gtk.IsWidget a, Gtk.IsContainer a) => (Gtk.ManagedPtr a -> a) -> [AttrPair a] -> [Object] -> Object
container ctor attrs children = Object (GtkContainer ctor attrs children)
