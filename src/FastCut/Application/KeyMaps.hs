{-# LANGUAGE GADTs             #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedLabels  #-}
{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE OverloadedStrings #-}
module FastCut.Application.KeyMaps where

import           FastCut.Application.Base

import           FastCut.Composition.Insert
import           FastCut.Focus
import           FastCut.KeyMap

keymaps :: SMode m -> KeyMap (Command m)
keymaps =
  \case
    STimelineMode ->
      [ ([KeyChar 'h'], Mapping (FocusCommand FocusLeft))
      , ([KeyChar 'j'], Mapping (FocusCommand FocusDown))
      , ([KeyChar 'k'], Mapping (FocusCommand FocusUp))
      , ([KeyChar 'l'], Mapping (FocusCommand FocusRight))
      , ([KeyUp], Mapping (FocusCommand FocusUp))
      , ([KeyDown], Mapping (FocusCommand FocusDown))
      , ([KeyLeft], Mapping (FocusCommand FocusLeft))
      , ([KeyRight], Mapping (FocusCommand FocusRight))
      , ([KeyChar 'i'], Mapping Import)
      , ([KeyChar 'r'], Mapping Render)
      , ( [KeyChar 'p']
        , SequencedMappings
            [ ([KeyChar 'c'], Mapping (InsertCommand InsertClip LeftOf))
            , ([KeyChar 'g'], Mapping (InsertCommand InsertGap LeftOf))
            , ([KeyChar 'p'], Mapping (InsertCommand InsertComposition LeftOf))
            ])
      , ( [KeyChar 'P']
        , SequencedMappings
            [ ([KeyChar 'c'], Mapping (InsertCommand InsertClip LeftMost))
            , ([KeyChar 'g'], Mapping (InsertCommand InsertGap LeftMost))
            , ( [KeyChar 'p']
              , Mapping (InsertCommand InsertComposition LeftMost))
            ])
      , ( [KeyChar 'a']
        , SequencedMappings
            [ ([KeyChar 'c'], Mapping (InsertCommand InsertClip RightOf))
            , ([KeyChar 'g'], Mapping (InsertCommand InsertGap RightOf))
            , ([KeyChar 'p'], Mapping (InsertCommand InsertComposition RightOf))
            ])
      , ( [KeyChar 'A']
        , SequencedMappings
            [ ([KeyChar 'c'], Mapping (InsertCommand InsertClip RightMost))
            , ([KeyChar 'g'], Mapping (InsertCommand InsertGap RightMost))
            , ( [KeyChar 'p']
              , Mapping (InsertCommand InsertComposition RightMost))
            ])
      , ([KeyChar 'd'], Mapping Delete)
      , ([KeyChar '?'], Mapping Help)
      , ([KeyChar 'q'], Mapping Exit)
      ]
    SLibraryMode ->
      [ ([KeyChar 'q'], Mapping Cancel)
      , ([KeyEscape], Mapping Cancel)
      , ([KeyChar '?'], Mapping Help)
      ]
    SImportMode ->
      [ ([KeyChar 'q'], Mapping Cancel)
      , ([KeyEscape], Mapping Cancel)
      , ([KeyChar '?'], Mapping Help)
      ]
