{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE OverloadedLabels  #-}
{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE OverloadedStrings #-}
module Komposition.UserInterface.GtkInterface.HelpView () where

import           Komposition.Prelude       hiding (State, on)

import           GI.Gtk                    (Align (..), Box (..), Label (..),
                                            Orientation (..),
                                            ScrolledWindow (..), Window (..))
import           GI.Gtk.Declarative        as Gtk

import           Komposition.KeyMap
import           Komposition.UserInterface hiding (Window)
import           Komposition.UserInterface.Help
import           Komposition.UserInterface.GtkInterface.GtkWindowMarkup

instance HelpView GtkWindowMarkup where
  helpView keymaps =
    GtkWindowMarkup $
      bin Window [on #deleteEvent (const (True, HelpClosed))]
        $ bin ScrolledWindow [classes ["help"]]
        $ container
            Box
            [#orientation := OrientationVertical, classes ["help-container"]]
        $ do
            boxChild False False 10
              $ widget Label [#label := "Key Bindings", classes ["heading"]]
            mapM_ (boxChild True True 10 . keymapTable) keymaps

keymapTable :: ModeKeyMap -> Widget HelpEvent
keymapTable (ModeKeyMap mode keymap) =
  container Box [#orientation := OrientationVertical, classes ["key-map"]] $ do
    boxChild False False 10
      $ widget Label [#label := modeTitle mode, classes ["subheading"]]
    mapM_ (boxChild False False 0 . keySequenceEntry) (sequences keymap)

keySequenceEntry :: (KeySequence, Command mode) -> Widget HelpEvent
keySequenceEntry (keySequence, cmd) =
  container Box
            [#orientation := OrientationHorizontal, classes ["key-map-entry"]]
    $ do
        boxChild False False 0 $ container Box [] $ mapM_
          (boxChild False False 0 . keyComboLabel)
          keySequence
        boxChild
          True
          True
          0
          (widget
            Label
            [#label := commandName cmd, #halign := AlignEnd, classes ["name"]]
          )

keyComboLabel :: KeyCombo -> Widget HelpEvent
keyComboLabel combo =
  widget Label [#label := keyComboToText combo, classes ["key-combo"]]
