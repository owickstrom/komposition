{-# LANGUAGE OverloadedLabels      #-}
{-# LANGUAGE OverloadedLists       #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE GADTs                 #-}
module FastCut.Application.KeyMaps where

import FastCut.Application.Base

import           FastCut.KeyMap
import           FastCut.Focus
import           FastCut.Composition.Insert

keymaps :: SMode m -> KeyMap (Command m)
keymaps = \case
  STimelineMode ->
    [ ([KeyChar 'h'], Mapping (FocusCommand FocusLeft))
    , ([KeyChar 'j'], Mapping (FocusCommand FocusDown))
    , ([KeyChar 'k'], Mapping (FocusCommand FocusUp))
    , ([KeyChar 'l'], Mapping (FocusCommand FocusRight))
    , ([KeyChar 'i'], Mapping Import)
    , ([KeyChar 'r'], Mapping Render)
    , ( [KeyChar 'p']
      , SequencedMappings
        [ ([KeyChar 'c'], Mapping (InsertCommand InsertClip LeftOf))
        , ([KeyChar 'g'], Mapping (InsertCommand InsertGap LeftOf))
        , ([KeyChar 'p'], Mapping (InsertCommand InsertComposition LeftOf))
        ]
      )
    , ( [KeyChar 'P']
      , SequencedMappings
        [ ([KeyChar 'c'], Mapping (InsertCommand InsertClip LeftMost))
        , ([KeyChar 'g'], Mapping (InsertCommand InsertGap LeftMost))
        , ([KeyChar 'p'], Mapping (InsertCommand InsertComposition LeftMost))
        ]
      )
    , ( [KeyChar 'a']
      , SequencedMappings
        [ ([KeyChar 'c'], Mapping (InsertCommand InsertClip RightOf))
        , ([KeyChar 'g'], Mapping (InsertCommand InsertGap RightOf))
        , ([KeyChar 'p'], Mapping (InsertCommand InsertComposition RightOf))
        ]
      )
    , ( [KeyChar 'A']
      , SequencedMappings
        [ ([KeyChar 'c'], Mapping (InsertCommand InsertClip RightMost))
        , ([KeyChar 'g'], Mapping (InsertCommand InsertGap RightMost))
        , ([KeyChar 'p'], Mapping (InsertCommand InsertComposition RightMost))
        ]
      )
    , ([KeyChar 'd'], Mapping Delete)
    , ([KeyChar '?'], Mapping Help)
    , ([KeyChar 'q'], Mapping Exit)
    ]
  SLibraryMode ->
    [ ([KeyChar 'j'], Mapping LibraryDown)
    , ([KeyChar 'k'], Mapping LibraryUp)
    , ([KeyChar 'q'], Mapping Cancel)
    , ([KeyChar '?'], Mapping Help)
    , ([KeyEnter]   , Mapping LibrarySelect)
    ]
  SImportMode ->
    [([KeyChar 'q'], Mapping Cancel), ([KeyChar '?'], Mapping Help)]

