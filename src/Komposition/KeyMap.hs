{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveFunctor     #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}
-- | A 'KeyMap' contains the key bindings commands, and can be
-- introspected to display help messages.

module Komposition.KeyMap where

import           Komposition.Prelude hiding (toList)

import           Data.HashMap.Strict (HashMap)
import           Data.HashSet        (HashSet)
import qualified Data.HashSet        as HashSet
import qualified Data.Text           as Text
import           GHC.Exts            (IsList, Item, fromList, toList)

data Modifier
  = Ctrl
  | Shift
  | Meta
  deriving (Show, Eq, Generic, Hashable)

data Key
  = KeyChar Char
  | KeyModifier Modifier
  | KeyEnter
  | KeySpace
  | KeyEscape
  | KeyUp
  | KeyDown
  | KeyLeft
  | KeyRight
  deriving (Show, Eq, Generic, Hashable)

type KeyCombo = HashSet Key

type KeySequence = [KeyCombo]

newtype KeyMap a = KeyMap (HashMap KeyCombo (KeyMapEntry a))
  deriving (Show, Eq, Functor)

instance IsList (KeyMap a) where
  type Item (KeyMap a) = (KeyCombo, KeyMapEntry a)
  fromList = KeyMap . fromList
  toList (KeyMap m) = toList m

data KeyMapEntry a = Mapping a | SequencedMappings (KeyMap a)
  deriving (Show, Eq, Functor)

sequences :: Ord a => KeyMap a -> [(KeySequence, a)]
sequences = sortBy cmdOrdering . flattenKeyMap []
 where
  flattenKeyMap prefix (KeyMap entries) =
    concatMap (flattenEntry prefix) (toList entries)
  flattenEntry prefix (keys, Mapping x) = [(prefix <> [keys], x)]
  flattenEntry prefix (keys, SequencedMappings km) =
    flattenKeyMap (prefix <> [keys]) km
  cmdOrdering (_, c1) (_, c2) = c1 `compare` c2

lookupSequence :: (Eq a, Ord a) => a -> KeyMap a -> [KeySequence]
lookupSequence x keymap = map fst (filter ((== x) . snd) (sequences keymap))

keySequenceToText :: KeySequence -> Text
keySequenceToText = Text.unwords . map keyComboToText

keyComboToText :: KeyCombo -> Text
keyComboToText = Text.intercalate "-" .
                 map keyToText . HashSet.toList

keyToText :: Key -> Text
keyToText =
  \case
    KeyChar c     -> Text.singleton c
    KeyModifier m -> modifierToText m
    KeyEnter      -> "<return>"
    KeySpace      -> "<space>"
    KeyEscape     -> "<escape>"
    KeyUp         -> "<up>"
    KeyDown       -> "<down>"
    KeyLeft       -> "<left>"
    KeyRight      -> "<right>"

modifierToText :: Modifier -> Text
modifierToText = \case
  Ctrl  -> "C"
  Shift -> "S"
  Meta  -> "M"
