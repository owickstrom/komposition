{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE OverloadedLabels  #-}
{-# LANGUAGE OverloadedStrings #-}

module Komposition.UserInterface.GtkInterface.HelpView
  ( helpView
  )
where

import           Komposition.Prelude       hiding (State, on)

import           GI.Gtk                    (Align (..), Box (..), Label (..),
                                            Orientation (..),
                                            ScrolledWindow (..), Window (..))
import           GI.Gtk.Declarative        as Gtk

import           Komposition.KeyMap
import           Komposition.UserInterface hiding (Window, helpView)

helpView :: Typeable mode => [ModeKeyMap] -> Bin Window Widget (Event mode)
helpView keymaps =
  bin Window []
    $ bin ScrolledWindow []
    $ container
        Box
        [#orientation := OrientationVertical, classes ["help-container"]]
    $ do
        boxChild False False 10
          $ widget Label [#label := "Key Bindings", classes ["heading"]]
        mapM_ (boxChild True True 10 . keymapTable) keymaps

keymapTable :: Typeable mode => ModeKeyMap -> Widget (Event mode)
keymapTable (ModeKeyMap mode keymap) =
  container Box [#orientation := OrientationVertical, classes ["key-map"]] $ do
    boxChild False False 10
      $ widget Label [#label := modeTitle mode, classes ["subheading"]]
    mapM_ (boxChild False False 0 . keySequenceEntry) (sequences keymap)

keySequenceEntry
  :: Typeable mode => (KeySequence, Command mode') -> Widget (Event mode)
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

keyComboLabel :: Typeable mode => KeyCombo -> Widget (Event mode)
keyComboLabel combo =
  widget Label [#label := keyComboToText combo, classes ["key-combo"]]
