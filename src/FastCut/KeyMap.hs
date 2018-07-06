{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveFunctor  #-}
{-# LANGUAGE DeriveGeneric  #-}
{-# LANGUAGE TypeFamilies   #-}
-- | A 'KeyMap' contains the key bindings commands, and can be
-- introspected to display help messages.

module FastCut.KeyMap where

import           FastCut.Prelude     hiding (toList)

import           Data.Hashable
import           Data.HashMap.Strict (HashMap)
import           Data.HashSet        (HashSet)
import           GHC.Exts            (IsList, Item, fromList, toList)
import           GHC.Generics

data Modifier
  = Ctrl
  | Shift
  | Meta

data Key
  = KeyChar Char
  | KeyModifier
  | KeyEnter
  deriving (Show, Eq, Generic, Hashable)

type KeyCombo = HashSet Key

newtype KeyMap a = KeyMap (HashMap KeyCombo (KeyMapEntry a))
  deriving (Show, Eq, Functor)

instance IsList (KeyMap a) where
  type Item (KeyMap a) = (KeyCombo, KeyMapEntry a)
  fromList = KeyMap . fromList
  toList (KeyMap m) = toList m

data KeyMapEntry a = Mapping a | SequencedMappings (KeyMap a)
  deriving (Show, Eq, Functor)
