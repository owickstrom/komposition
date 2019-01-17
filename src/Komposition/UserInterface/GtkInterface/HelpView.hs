{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE OverloadedLabels  #-}
{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE OverloadedStrings #-}
module Komposition.UserInterface.GtkInterface.HelpView () where

import qualified Data.Vector                                            as Vector
import           Komposition.Prelude                                    hiding
                                                                         (State,
                                                                         on)

import           GI.Gtk                                                 (Align (..),
                                                                         Box (..),
                                                                         Dialog (..),
                                                                         Label (..),
                                                                         Orientation (..),
                                                                         ScrolledWindow (..),
                                                                         Window (..))
import           GI.Gtk.Declarative                                     as Gtk

import           Komposition.KeyMap
import           Komposition.UserInterface                              hiding
                                                                         (Window)
import           Komposition.UserInterface.GtkInterface.GtkWindowMarkup
import           Komposition.UserInterface.Help

instance HelpView GtkWindowMarkup where
  helpView keymaps =
    GtkModalMarkup $
      bin Window [on #deleteEvent (const (True, HelpClosed))]
        $ bin ScrolledWindow [classes ["help"]]
        $ container
            Box
            [#orientation := OrientationVertical, classes ["help-container"]]
        (
            BoxChild defaultBoxChildProperties { expand = False, fill = False, padding = 10 } heading
            `Vector.cons`
            fmap (BoxChild defaultBoxChildProperties { expand = True, fill = True, padding = 10 } . keymapTable) (Vector.fromList keymaps)
        )
    where
      heading = widget Label [#label := "Key Bindings", classes ["heading"]]

keymapTable :: ModeKeyMap -> Widget HelpEvent
keymapTable (ModeKeyMap mode keymap) = container
  Box
  [#orientation := OrientationVertical, classes ["key-map"]]
  (             BoxChild
      defaultBoxChildProperties { expand = False, fill = False, padding = 10 }
      heading
  `Vector.cons` fmap
                  ( BoxChild defaultBoxChildProperties { expand  = False
                                                       , fill    = False
                                                       , padding = 0
                                                       }
                  . keySequenceEntry
                  )
                  (Vector.fromList (sequences keymap))
  )
  where
    heading = widget Label [#label := modeTitle mode, classes ["subheading"]]

keySequenceEntry :: (KeySequence, Command mode) -> Widget HelpEvent
keySequenceEntry (keySequence, cmd) = container
  Box
  [#orientation := OrientationHorizontal, classes ["key-map-entry"]]
  [ BoxChild defaultBoxChildProperties { expand  = False
                                       , fill    = False
                                       , padding = 0
                                       }
    $ container
        Box
        []
        (fmap
          ( BoxChild defaultBoxChildProperties { expand  = False
                                               , fill    = False
                                               , padding = 0
                                               }
          . keyComboLabel
          )
          (Vector.fromList keySequence)
        )
  , BoxChild
    defaultBoxChildProperties { expand = True, fill = True, padding = 0 }
    (widget Label
            [#label := commandName cmd, #halign := AlignEnd, classes ["name"]]
    )
  ]

keyComboLabel :: KeyCombo -> Widget HelpEvent
keyComboLabel combo =
  widget Label [#label := keyComboToText combo, classes ["key-combo"]]
