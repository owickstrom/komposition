{-# LANGUAGE DataKinds         #-}
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
import           FastCut.MediaType

insertBindings :: InsertPosition -> KeyMapEntry (Command 'TimelineMode)
insertBindings position =
  SequencedMappings
    [ ([KeyChar 'v'], mediaTypeBindings (Just Video))
    , ([KeyChar 'a'], mediaTypeBindings (Just Audio))
    , ([KeyChar 'c'], Mapping (InsertCommand (InsertClip Nothing) position))
    , ([KeyChar 'g'], Mapping (InsertCommand (InsertGap Nothing) position))
    , ([KeyChar 'p'], Mapping (InsertCommand InsertComposition LeftMost))
    ]
  where
    mediaTypeBindings mediaType' =
      SequencedMappings
        [ ( [KeyChar 'c']
          , Mapping (InsertCommand (InsertClip mediaType') position))
        , ( [KeyChar 'g']
          , Mapping (InsertCommand (InsertGap mediaType') position))
        ]

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
      , ([KeySpace], Mapping Preview)
      , ([KeyChar 'p'], insertBindings LeftOf)
      , ([KeyChar 'P'], insertBindings LeftMost)
      , ([KeyChar 'a'], insertBindings RightOf)
      , ([KeyChar 'A'], insertBindings RightMost)
      , ([KeyChar 'd'], Mapping Delete)
      , ([KeyChar 's'], Mapping Split)
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
