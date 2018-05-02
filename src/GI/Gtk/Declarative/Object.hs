{-# LANGUAGE GADTs               #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving  #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeOperators       #-}

-- | An 'Object' wraps any 'Patchable', providing a type-constrained
-- equivalent of a 'Dynamic' value. It is used to support
-- heterogeneous declarative objects in container objects, and to
-- support equailty checks on different types of objects when
-- calculating patches.

module GI.Gtk.Declarative.Object where

import           Data.Typeable

import           GI.Gtk.Declarative.Patch

data Object where
  Object :: (Eq o, Show o, Typeable o, Patchable o) => o -> Object

instance Patchable Object where
  create (Object obj) = create obj
  patch (Object (o1 :: t1)) (Object (o2 :: t2)) =
    case eqT @t1 @t2 of
      Just Refl -> patch o1 o2
      Nothing   -> Replace (create o2)

deriving instance Show Object

instance Eq Object where
  Object (a :: o) == Object (b :: n) =
    case eqT @o @n  of
      Just Refl -> a == b
      Nothing   -> False
