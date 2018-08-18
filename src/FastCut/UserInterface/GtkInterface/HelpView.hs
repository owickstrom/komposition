{-# OPTIONS_GHC -fno-warn-unticked-promoted-constructors #-}
{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE OverloadedLabels  #-}
{-# LANGUAGE OverloadedStrings #-}

module FastCut.UserInterface.GtkInterface.HelpView
  ( helpView
  )
where

import           FastCut.Prelude                   hiding ( State
                                                          , on
                                                          )

import           GI.Gtk.Declarative            as Gtk

import           FastCut.UserInterface
import           FastCut.KeyMap

helpView :: [ModeKeyMap] -> Markup
helpView keymaps = container
  ScrolledWindow
  []
  (container
    Box
    [#orientation := OrientationVertical, classes ["help-container"]]
    ( BoxChild False
               False
               10
               (node Label [#label := "Key Bindings", classes ["heading"]])
    : map (BoxChild True True 10 . keymapTable) keymaps
    )
  )

keymapTable :: ModeKeyMap -> Markup
keymapTable (ModeKeyMap mode keymap) = container
  Box
  [#orientation := OrientationVertical, classes ["key-map"]]
  ( BoxChild False
             False
             10
             (node Label [#label := modeTitle mode, classes ["subheading"]])
  : map (BoxChild False False 0 . keySequenceEntry) (sequences keymap)
  )

keySequenceEntry :: (KeySequence, Command mode) -> Markup
keySequenceEntry (keySequence, cmd) = container
  Box
  [#orientation := OrientationHorizontal, classes ["key-map-entry"]]
  [ BoxChild False False 0 (container Box [] (map keyComboLabel keySequence))
  , BoxChild
    True
    True
    0
    (node Label
          [#label := commandName cmd, #halign := AlignEnd, classes ["name"]]
    )
  ]

keyComboLabel :: KeyCombo -> Markup
keyComboLabel combo =
  node Label [#label := keyComboToText combo, classes ["key-combo"]]
