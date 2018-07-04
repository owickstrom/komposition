{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ConstraintKinds  #-}
{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE ExplicitForAll   #-}
{-# LANGUAGE LambdaCase       #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedLists  #-}
{-# LANGUAGE PolyKinds        #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE TypeFamilies     #-}
{-# LANGUAGE TypeOperators    #-}
-- |
module FastCut.Application where

import Prelude hiding ((>>), (>>=))

import           Control.Lens
import           Data.HashMap.Strict      (HashMap)
import qualified Data.HashMap.Strict      as HashMap
import           Data.Row.Records hiding  (map)
import           GHC.Exts                 (fromListN)
import           GHC.OverloadedLabels
import           Motor.FSM

import           Control.Monad.Indexed.IO
import           FastCut.Focus
import           FastCut.Project
import           FastCut.Sequence
import           FastCut.UserInterface

data TimelineEvent
  = FocusEvent FocusEvent
  | AppendClip
  | Exit

data LibraryEvent
  = LibraryEscape
  | LibraryUp
  | LibraryDown
  | LibrarySelect

(>>) :: IxMonad m => m i j a -> m j k b -> m i k b
(>>) = (>>>)

(>>=) :: IxMonad m => m i j a -> (a -> m j k b) -> m i k b
(>>=) = (>>>=)

nextEventOrBeep ::
     (UserInterface m, IxMonadIO m, HasType n (State m s) r)
  => HashMap KeyCombo e
  -> Name n
  -> m r r (Maybe e)
nextEventOrBeep keymap gui = do
  KeyPress combo <- nextEvent gui
  maybe
    (beep gui >>> ireturn Nothing)
    (ireturn . Just)
    (HashMap.lookup combo keymap)

nextLibraryEvent ::
     (UserInterface m, IxMonadIO m, HasType n (State m 'LibraryMode) r)
  => Name n
  -> m r r (Maybe LibraryEvent)
nextLibraryEvent =
  nextEventOrBeep
    [ ([KeyChar 'j'], LibraryDown)
    , ([KeyChar 'k'], LibraryUp)
    , ([KeyChar 'q'], LibraryEscape)
    , ([KeyEnter], LibrarySelect)
    ]

focusClip :: Int -> [Clip a mt] -> [Clip Focused mt]
focusClip i clips =
  clips
  & map (setClipAnnotation Blurred)
  & ix i %~ setClipAnnotation Focused

selectClipFromList ::
     ( UserInterface m
     , IxMonadIO m
     , lm ~ State m 'LibraryMode
     , HasType n lm r
     , Modify n lm r ~ r
     )
  => Name n
  -> [Clip () mt]
  -> Int
  -> m r r (Maybe (Clip () mt))
selectClipFromList gui clips n = do
  updateLibrary gui (focusClip n clips)
  nextLibraryEvent gui >>>= \case
    Just LibraryEscape -> ireturn Nothing
    Just LibrarySelect -> ireturn (clips ^? element n)
    Just LibraryUp
      | n > 0 -> selectClipFromList gui clips (pred n)
      | otherwise -> continue
    Just LibraryDown
      | n < length clips - 1 -> selectClipFromList gui clips (succ n)
      | otherwise -> continue
    Nothing -> continue
  where
    continue = do
      beep gui
      selectClipFromList gui clips n

selectClip ::
     ( UserInterface m
     , IxMonadIO m
     , lm ~ State m 'LibraryMode
     , tm ~ State m 'TimelineMode
     , HasType n tm i
     , HasType n tm o
     , (Modify n lm i .! n) ~ lm
     , Modify n tm (Modify n lm i) ~ o
     , Modify n lm (Modify n lm i) ~ Modify n lm i
     )
  => Name n
  -> Project
  -> Focus
  -> SMediaType mt
  -> m i o (Maybe (Clip () mt))
selectClip gui project focus' mediaType = do
  enterLibrary gui
  case mediaType of
    SVideo -> do
      clip <- selectClipFromList gui (project ^. library . videoClips) 0
      exitLibrary gui project focus'
      ireturn clip
    SAudio -> do
      clip <- selectClipFromList gui (project ^. library . audioClips) 0
      exitLibrary gui project focus'
      ireturn clip

nextTimelineEvent ::
     (UserInterface m, IxMonadIO m, HasType n (State m 'TimelineMode) r)
  => Name n
  -> m r r (Maybe TimelineEvent)
nextTimelineEvent =
  nextEventOrBeep
    [ ([KeyChar 'h'], FocusEvent FocusLeft)
    , ([KeyChar 'j'], FocusEvent FocusDown)
    , ([KeyChar 'k'], FocusEvent FocusUp)
    , ([KeyChar 'l'], FocusEvent FocusRight)
    , ([KeyChar 'a'], AppendClip)
    , ([KeyChar 'q'], Exit)
    ]

timelineMode ::
     (UserInterface m, IxMonadIO m)
  => Name n
  -> Project
  -> Focus
  -> m (n .== State m 'TimelineMode) Empty ()
timelineMode gui project focus' = do
  updateTimeline gui project focus'
  nextTimelineEvent gui >>>= \case
    Just (FocusEvent e) ->
      case modifyFocus (project ^. topSequence) e focus' of
        Left _err      -> do
          beep gui
          timelineMode gui project focus'
        Right newFocus -> timelineMode gui project newFocus
    Just AppendClip ->
      selectClip gui project focus' SVideo >>= \case
        Just clip ->
          project
            & topSequence %~ appendAt' focus' (Right (Clip clip))
            & \p -> timelineMode gui p focus'
        Nothing ->
          timelineMode gui project focus'
    Just Exit -> exit gui
    Nothing -> do
      beep gui
      timelineMode gui project focus'

fastcut :: (IxMonadIO m) => UserInterface m => Project -> m Empty Empty ()
fastcut project = do
  start #gui project initialFocus
  timelineMode #gui project initialFocus
  where
    initialFocus = SubFocus 0 SequenceFocus
