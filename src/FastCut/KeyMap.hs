{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveFunctor     #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}
-- | A 'KeyMap' contains the key bindings commands, and can be
-- introspected to display help messages.

module FastCut.KeyMap where

import           FastCut.Prelude                   hiding ( toList )

import qualified Data.Text                     as Text
import           Data.HashMap.Strict                      ( HashMap )
import qualified Data.HashSet                  as HashSet
import           Data.HashSet                             ( HashSet )
import           GHC.Exts                                 ( IsList
                                                          , Item
                                                          , fromList
                                                          , toList
                                                          )

data Modifier
  = Ctrl
  | Shift
  | Meta
  deriving (Show, Eq, Generic, Hashable)

data Key
  = KeyChar Char
  | KeyModifier Modifier
  | KeyEnter
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

sequences :: KeyMap a -> [(KeySequence, a)]
sequences = flattenKeyMap []
 where
  flattenKeyMap prefix (KeyMap entries) =
    concatMap (flattenEntry prefix) (toList entries)
  flattenEntry prefix (keys, Mapping x) = [(prefix <> [keys], x)]
  flattenEntry prefix (keys, SequencedMappings km) =
    flattenKeyMap (prefix <> [keys]) km


keySequenceToText :: KeySequence -> Text
keySequenceToText = Text.unwords . map keyComboToText

keyComboToText :: KeyCombo -> Text
keyComboToText = Text.intercalate "-" . map keyToText . HashSet.toList

keyToText :: Key -> Text
keyToText   = \case
  KeyChar     c -> Text.singleton c
  KeyModifier m -> modifierToText m
  KeyEnter      -> "<return>"

modifierToText :: Modifier -> Text
modifierToText = \case
  Ctrl  -> "C"
  Shift -> "S"
  Meta  -> "M"