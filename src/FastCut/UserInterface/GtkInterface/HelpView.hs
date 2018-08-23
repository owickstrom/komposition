{-# OPTIONS_GHC -fno-warn-unticked-promoted-constructors #-}
{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE OverloadedLabels  #-}
{-# LANGUAGE OverloadedStrings #-}

module FastCut.UserInterface.GtkInterface.HelpView
  ( helpView
  )
where

import           FastCut.Prelude       hiding (State, on)

import           GI.Gtk                (Box (..), Label (..), Orientation (..),
                                        ScrolledWindow (..), Align(..))
import           GI.Gtk.Declarative    as Gtk

import           FastCut.KeyMap
import           FastCut.UserInterface

helpView :: Typeable mode => [ModeKeyMap] -> Widget (Event mode)
helpView keymaps = container ScrolledWindow [] scrolledArea
 where
  scrolledArea :: Typeable mode => Widget (Event mode)
  scrolledArea =
    container
        Box
        [#orientation := OrientationVertical, classes ["help-container"]]
      $ do
          boxChild False False 10
            $ node Label [#label := "Key Bindings", classes ["heading"]]
          mapM_ (boxChild True True 10 . keymapTable) keymaps


keymapTable :: Typeable mode => ModeKeyMap -> Widget (Event mode)
keymapTable (ModeKeyMap mode keymap) = container
  Box
  [#orientation := OrientationVertical, classes ["key-map"]]
  children
 where
  children :: Typeable mode => MarkupOf BoxChild (Event mode) ()
  children = do
    boxChild False False 10
      $ node Label [#label := modeTitle mode, classes ["subheading"]]
    mapM_ (boxChild False False 0 . keySequenceEntry) (sequences keymap)

keySequenceEntry
  :: Typeable mode => (KeySequence, Command mode') -> Widget (Event mode)
keySequenceEntry (keySequence, cmd) = container
  Box
  [#orientation := OrientationHorizontal, classes ["key-map-entry"]]
  children
 where
  children :: Typeable mode => MarkupOf BoxChild (Event mode) ()
  children = do
    boxChild False False 0 (container Box [] keyLabels)
    boxChild
      True
      True
      0
      (node Label
            [#label := commandName cmd, #halign := AlignEnd, classes ["name"]]
      )
  keyLabels :: Typeable mode => MarkupOf BoxChild (Event mode) ()
  keyLabels =
    mapM_ (boxChild False False 0 . keyComboLabel) keySequence

keyComboLabel :: Typeable mode => KeyCombo -> Widget (Event mode)
keyComboLabel combo =
  node Label [#label := keyComboToText combo, classes ["key-combo"]]
