{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators         #-}

-- | A 'Node' represents a declarative "leaf" object, i.e. one that is
-- not a container with children.

module GI.Gtk.Declarative.Node
  ( Node
  , node
  ) where

import           Data.Either               (partitionEithers)
import           Data.Foldable             (fold)
import qualified Data.GI.Base.Attributes   as GI
import           Data.Typeable
import qualified GI.Gtk                    as Gtk

import           GI.Gtk.Declarative.CSS
import           GI.Gtk.Declarative.Object
import           GI.Gtk.Declarative.Patch
import           GI.Gtk.Declarative.Props

data Node a where
  Node :: (Typeable a, Gtk.IsWidget a) => (Gtk.ManagedPtr a -> a) -> [PropPair a] -> Node a

instance Eq (Node a) where
  Node _ a1 == Node _ a2 =
    a1 == a2

instance Show (Node a) where
  show = \case
    Node{} -> "Node"

instance Patchable (Node a) where
  create = \case
    (Node ctor attrs) -> do
        let (attrOps, classSets) = partitionEithers (map toOpOrClass attrs)
        widget <- Gtk.new ctor attrOps
        addClasses widget (fold classSets)
        Gtk.toWidget widget
    where
      toOpOrClass :: PropPair obj -> Either (GI.AttrOp obj 'GI.AttrConstruct) ClassSet
      toOpOrClass =
        \case
          (attr := value) -> Left (attr Gtk.:= value)
          Classes c -> Right c
  patch (Node _ oldAttrs) (Node ctor newAttrs) = Modify $ \widget -> do
    let (_, oldClassSets) = partitionEithers (map toOpOrClass oldAttrs)
        (newAttrOps, newClassSets) = partitionEithers (map toOpOrClass newAttrs)
    w <- Gtk.unsafeCastTo ctor widget
    Gtk.set w newAttrOps
    replaceClasses w (fold oldClassSets) (fold newClassSets)
    Gtk.widgetShowAll w
    where
      toOpOrClass :: PropPair obj -> Either (GI.AttrOp obj 'GI.AttrSet) ClassSet
      toOpOrClass =
        \case
          (attr := value) -> Left (attr Gtk.:= value)
          Classes c -> Right c

node :: (Typeable a, Gtk.IsWidget a) => (Gtk.ManagedPtr a -> a) -> [PropPair a] -> Object
node ctor attrs = Object (Node ctor attrs)
