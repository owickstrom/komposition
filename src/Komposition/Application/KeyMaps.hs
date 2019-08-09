{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE GADTs            #-}
{-# LANGUAGE LambdaCase       #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedLists  #-}
module Komposition.Application.KeyMaps where

import           Komposition.Application.Base

import           Komposition.Composition.Insert
import           Komposition.Composition.Paste
import           Komposition.Focus
import           Komposition.KeyMap
import           Komposition.MediaType
import           Komposition.UserInterface

addBindings :: InsertPosition -> KeyMapEntry (Command 'TimelineMode)
addBindings position = SequencedMappings
  [ ([KeyChar 'v'], mediaTypeBindings (Just Video))
  , ([KeyChar 'a'], mediaTypeBindings (Just Audio))
  , ([KeyChar 'c'], Mapping (InsertCommand (InsertClip Nothing) position))
  , ([KeyChar 'g'], Mapping (InsertCommand (InsertGap Nothing) position))
  , ([KeyChar 'p'], Mapping (InsertCommand InsertComposition position))
  ]
  where
    mediaTypeBindings mediaType' = SequencedMappings
      [ ( [KeyChar 'c']
        , Mapping (InsertCommand (InsertClip mediaType') position)
        )
      , ([KeyChar 'g'], Mapping (InsertCommand (InsertGap mediaType') position))
      ]

keymaps :: SMode m -> KeyMap (Command m)
keymaps = \case
  SWelcomeScreenMode ->
    [ ([KeyChar 'q'], Mapping Cancel)
    , ([KeyEscape]  , Mapping Cancel)
    , ([KeyChar '?'], Mapping Help)
    ]
  SNewProjectMode ->
    [ ([KeyChar 'q'], Mapping Cancel)
    , ([KeyEscape]  , Mapping Cancel)
    , ([KeyChar '?'], Mapping Help)
    ]
  STimelineMode ->
    [ ([KeyChar 'h']                  , Mapping (FocusCommand FocusLeft))
    , ([KeyChar 'j']                  , Mapping (FocusCommand FocusDown))
    , ([KeyChar 'k']                  , Mapping (FocusCommand FocusUp))
    , ([KeyChar 'l']                  , Mapping (FocusCommand FocusRight))
    , ([KeyUp]                        , Mapping (FocusCommand FocusUp))
    , ([KeyDown]                      , Mapping (FocusCommand FocusDown))
    , ([KeyLeft]                      , Mapping (FocusCommand FocusLeft))
    , ([KeyRight]                     , Mapping (FocusCommand FocusRight))
    , ([KeySpace]                     , Mapping PlayOrStop)
    , ([KeyChar 'a']                  , addBindings RightOf)
    , ([KeyChar 'A']                  , addBindings LeftOf)
    , ([KeyChar 'd']                  , Mapping Delete)
    , ([KeyChar 'p']                  , Mapping (Paste PasteRightOf))
    , ([KeyChar 'P']                  , Mapping (Paste PasteLeftOf))
    , ([KeyChar 'y']                  , Mapping Copy)
    , ([KeyChar 's']                  , Mapping Split)
    , ([KeyChar 'm']                  , Mapping Join)
    , ([KeyChar 'i']                  , Mapping Import)
    , ([KeyChar 'u']                  , Mapping Undo)
    , ([KeyModifier Ctrl, KeyChar 'r'], Mapping Redo)
    , ([KeyChar '?']                  , Mapping Help)
    , ([KeyChar 'q']                  , Mapping Exit)
    ]
  SLibraryMode ->
    [ ([KeyChar 'q'], Mapping Cancel)
    , ([KeyEscape]  , Mapping Cancel)
    , ([KeyChar '?'], Mapping Help)
    ]
  SImportMode ->
    [ ([KeyChar 'q'], Mapping Cancel)
    , ([KeyEscape]  , Mapping Cancel)
    , ([KeyChar '?'], Mapping Help)
    ]
