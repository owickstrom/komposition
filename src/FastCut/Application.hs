{-# OPTIONS_GHC -fno-warn-unticked-promoted-constructors #-}

{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE ConstraintKinds   #-}
{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE ExplicitForAll    #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedLabels  #-}
{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds         #-}
{-# LANGUAGE RebindableSyntax  #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE TypeOperators     #-}
module FastCut.Application where

import           FastCut.Prelude hiding ((>>), (>>=), State, cancel)

import           Control.Lens
import           Data.String (fromString)
import           Data.Row.Records hiding  (map)
import           GHC.Exts                 (fromListN)
import           Motor.FSM
import           Text.Printf

import           Control.Monad.Indexed.IO
import           FastCut.Focus
import           FastCut.KeyMap
import           FastCut.Project
import           FastCut.Composition
import           FastCut.Composition.Focused
import           FastCut.Composition.Insert
import           FastCut.UserInterface

(>>) :: IxMonad m => m i j a -> m j k b -> m i k b
(>>) = (>>>)

(>>=) :: IxMonad m => m i j a -> (a -> m j k b) -> m i k b
(>>=) = (>>>=)

keymaps :: SMode m -> KeyMap (Event m)
keymaps =
  fmap CommandKeyMappedEvent .
  \case
    STimelineMode ->
      [ ([KeyChar 'h'], Mapping (FocusCommand FocusLeft))
      , ([KeyChar 'j'], Mapping (FocusCommand FocusDown))
      , ([KeyChar 'k'], Mapping (FocusCommand FocusUp))
      , ([KeyChar 'l'], Mapping (FocusCommand FocusRight))
      , ([KeyChar 'i'], Mapping Import)
      , ( [KeyChar 'a']
        , SequencedMappings
            [ ([KeyChar 'c'], Mapping (AppendCommand AppendClip))
            , ([KeyChar 'g'], Mapping (AppendCommand AppendGap))
            , ([KeyChar 'p'], Mapping (AppendCommand AppendComposition))
            ])
      , ([KeyChar 'q'], Mapping Exit)
      ]
    SLibraryMode ->
      [ ([KeyChar 'j'], Mapping LibraryDown)
      , ([KeyChar 'k'], Mapping LibraryUp)
      , ([KeyChar 'q'], Mapping Cancel)
      , ([KeyEnter], Mapping LibrarySelect)
      ]
    SImportMode ->
      [ ([KeyChar 'q'], Mapping Cancel)
      ]
    SExitingMode ->
      [ ([KeyChar 'y'], Mapping ConfirmExit)
      , ([KeyChar 'n'], Mapping Cancel)
      ]

focusClip :: Int -> [Clip a mt] -> [Clip Focused mt]
focusClip i clips =
  clips
  & map (setClipAnnotation Blurred)
  & ix i %~ setClipAnnotation Focused

selectClipFromList ::
     (UserInterface m, IxMonadIO m, Modify n (State m LibraryMode) r ~ r)
  => Name n
  -> [Clip () mt]
  -> Int
  -> Actions m '[ n := Remain (State m LibraryMode)] r (Maybe (Clip () mt))
selectClipFromList gui clips n = do
  updateLibrary gui (focusClip n clips)
  nextEvent gui >>>= \case
    CommandKeyMappedEvent Cancel -> ireturn Nothing
    CommandKeyMappedEvent LibrarySelect -> ireturn (clips ^? element n)
    CommandKeyMappedEvent LibraryUp
      | n > 0 -> selectClipFromList gui clips (pred n)
      | otherwise -> continue
    CommandKeyMappedEvent LibraryDown
      | n < length clips - 1 -> selectClipFromList gui clips (succ n)
      | otherwise -> continue
  where
    continue = selectClipFromList gui clips n

-- | Convenient type for actions that transition from one mode
-- into another mode, doing some user interactions, and returning back
-- to the first mode with a value.
type ThroughMode base through n a
   = forall m i o lm tm.
   ( UserInterface m
     , IxMonadIO m
     , HasType n tm i
     , HasType n tm o
     , (Modify n lm i .! n) ~ lm
     , Modify n tm (Modify n lm i) ~ o
     , Modify n lm (Modify n lm i) ~ Modify n lm i
     , lm ~ State m through
     , tm ~ State m base
     )
   => m i o a

selectClip ::
  Name n
  -> Project
  -> Focus ft
  -> SMediaType mt
  -> ThroughMode TimelineMode LibraryMode n (Maybe (Clip () mt))
selectClip gui project focus' mediaType = do
  enterLibrary gui
  case mediaType of
    SVideo -> do
      clip <- selectClipFromList gui (project ^. library . videoClips) 0
      returnToTimeline gui project focus'
      ireturn clip
    SAudio -> do
      clip <- selectClipFromList gui (project ^. library . audioClips) 0
      returnToTimeline gui project focus'
      ireturn clip

selectClipAndAppend ::
  Name n
  -> Project
  -> Focus ft
  -> SMediaType mt
  -> ThroughMode TimelineMode LibraryMode n Project
selectClipAndAppend gui project focus' mediaType =
  selectClip gui project focus' mediaType >>= \case
    Just clip ->
      project
      & timeline %~ insert_ focus' (insertionOf clip) RightOf
      & ireturn
    Nothing -> ireturn project
  where
    insertionOf clip = case mediaType of
      SVideo -> InsertVideoPart (Clip clip)
      SAudio -> InsertAudioPart (Clip clip)

importFile ::
  Name n
  -> Project
  -> Focus ft
  -> ThroughMode TimelineMode ImportMode n (Maybe FilePath)
importFile gui project focus' = do
  enterImport gui
  f <- awaitImportClick Nothing
  returnToTimeline gui project focus'
  ireturn f
  where
    awaitImportClick mf = do
      cmd <- nextEvent gui
      case (cmd, mf) of
        (CommandKeyMappedEvent Cancel, _) -> ireturn Nothing
        (ImportClicked, Just file) -> ireturn (Just file)
        (ImportClicked, Nothing) -> awaitImportClick Nothing
        (ImportFileSelected file, _) -> awaitImportClick (Just file)

prettyFocusedAt :: FocusedAt a -> Text
prettyFocusedAt =
  \case
    FocusedSequence {} -> "sequence"
    FocusedParallel {} -> "parallel"
    FocusedVideoPart {} -> "video track"
    FocusedAudioPart {} -> "audio track"

append ::
     (UserInterface m, IxMonadIO m)
  => Name n
  -> Project
  -> Focus ft
  -> AppendCommand
  -> m (n .== State m 'TimelineMode) Empty ()
append gui project focus' cmd =
  case (cmd, atFocus focus' (project ^. timeline)) of
    (AppendComposition, Just (FocusedSequence _)) ->
      selectClip gui project focus' SVideo >>= \case
        Just clip ->
          project &
          timeline %~
          insert_
            focus'
            (InsertParallel (Parallel () [Clip clip] [Gap () (durationOf clip)]))
            RightOf &
          timelineMode gui focus'
        Nothing -> continue
    (AppendClip, Just (FocusedVideoPart _)) ->
      selectClipAndAppend gui project focus' SVideo >>>= timelineMode gui focus'
    (AppendClip, Just (FocusedAudioPart _)) ->
      selectClipAndAppend gui project focus' SAudio >>>= timelineMode gui focus'
    (AppendGap, Just _) ->
      project & timeline %~ insert_ focus' (InsertVideoPart (Gap () 10)) RightOf &
      timelineMode gui focus'
    (c, Just f) -> do
      iliftIO
        (putStrLn
           ("Cannot perform " <> show c <> " when focused at " <>
            prettyFocusedAt f))
      continue
    (_, Nothing) -> do
      iliftIO (putStrLn ("Warning: focus is invalid." :: Text))
      continue
  where
    continue = timelineMode gui focus' project

timelineMode ::
     (UserInterface m, IxMonadIO m)
  => Name n
  -> Focus ft
  -> Project
  -> m (n .== State m 'TimelineMode) Empty ()
timelineMode gui focus' project = do
  updateTimeline gui project focus'
  nextEvent gui >>>= \case
    CommandKeyMappedEvent (FocusCommand cmd) ->
      case modifyFocus (project ^. timeline) cmd focus' of
        Left err -> do
          printUnexpectedFocusError err cmd
          continue
        Right newFocus -> timelineMode gui newFocus project
    CommandKeyMappedEvent (AppendCommand cmd) -> append gui project focus' cmd
    CommandKeyMappedEvent Import ->
      importFile gui project focus' >>>= \case
        Just file -> do
          iliftIO (putStrLn ("Importing file: " <> file))
          continue
        Nothing -> continue
    CommandKeyMappedEvent Cancel -> continue
    CommandKeyMappedEvent Exit ->
      gui `confirmExitOr` (returnToTimeline gui project focus' >>> continue)
  where
    continue = timelineMode gui focus' project
    printUnexpectedFocusError err cmd =
      case err of
        UnhandledFocusModification {} ->
          iliftIO
            (printf
               "Error: could not handle focus modification %s\n"
               (show cmd :: Text))
        _ -> ireturn ()

confirmExitOr
  :: (UserInterface m, IxMonadIO m)
  => Name n
  -> m (n .== State m ExitingMode) Empty ()
  -> m (n .== State m TimelineMode) Empty ()
confirmExitOr gui cancel = do
  enterConfirmExit gui
  nextEvent gui >>>= \(CommandKeyMappedEvent cmd) ->
    case cmd of
      ConfirmExit -> exit gui
      Cancel -> cancel

fastcut :: (IxMonadIO m) => UserInterface m => Project -> m Empty Empty ()
fastcut project = do
  start #gui keymaps project initialFocus
  timelineMode #gui initialFocus project
  where
    initialFocus = SequenceFocus 0 Nothing
