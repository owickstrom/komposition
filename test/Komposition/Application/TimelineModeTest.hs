{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE PolyKinds        #-}
module Komposition.Application.TimelineModeTest where

import           Komposition.Prelude

import           Control.Effect
import           Control.Lens
import           Data.Row.Records                            (Empty)
import           Data.Tree                                   (drawTree)
import qualified Data.Vector                                 as Vector
import           Hedgehog                                    hiding (Command)
import qualified Hedgehog.Gen                                as Gen hiding
                                                                     (parallel)
import qualified Hedgehog.Range                              as Range
import           Motor.FSM                                   (ireturn, (>>>),
                                                              (>>>=))

import           Komposition.Application.Base                (Application)
import           Komposition.Application.KeyMaps
import           Komposition.Application.TimelineMode
import           Komposition.Composition                     (Timeline)
import           Komposition.Focus
import           Komposition.Project
import qualified Komposition.UndoRedo                        as UndoRedo
import           Komposition.UserInterface                   hiding (TimelineViewModel (..),
                                                              project)

import qualified Komposition.Composition.Generators          as Gen
import           Komposition.Composition.ToTree
import           Komposition.Import.Audio.StubAudioImport
import           Komposition.Import.Video.StubVideoImport
import           Komposition.Logging.StubLogger
import qualified Komposition.Project.Generators              as Gen
import           Komposition.Project.InMemoryProjectStore
import           Komposition.Render.StubRender
import           Komposition.UserInterface.StubUserInterface


initializeState :: MonadGen m => (Timeline (), Focus SequenceFocusType) -> m TimelineState
initializeState (timeline', focus')= do
  existingProject'    <-
    ExistingProject
    <$> (ProjectPath <$> Gen.string (Range.linear 1 10) Gen.unicode)
    <*> Gen.projectWithTimeline (pure timeline')
  pure TimelineState
    { _history  = UndoRedo.init (UndoableState existingProject' focus')
    , _statusMessage    = Nothing
    , _clipboard        = Nothing
    , _zoomLevel        = ZoomLevel 1
    , _previewImagePath = Nothing
    }

genUndoableTimelineCommand :: MonadGen m => m (Command 'TimelineMode)
genUndoableTimelineCommand =
  -- TODO: readd Split and Paste
  Gen.choice [pure Delete]

eventsVector =
  Vector.fromList . map (SomeEvent . CommandKeyMappedEvent)

runTimelineMode
  :: (Application t m sig, TimelineEffects sig) => TimelineState -> t m Empty Empty TimelineModeResult
runTimelineMode state' =
  newWindow #gui (timelineViewFromState state') keymap
    >>> timelineMode #gui state'
    >>>= \r -> destroyWindow #gui >>> ireturn r
  where keymap = CommandKeyMappedEvent <$> keymaps STimelineMode

runTimelineStubbedWithExit
  :: MonadTest m
  => [Command 'TimelineMode]
  -> TimelineState
  -> m TimelineState
runTimelineStubbedWithExit cmds state = case runPure state of
  Left  err                     -> annotateShow err >> failure
  Right TimelineClose           -> failure
  Right (TimelineExit endState) -> pure endState
  where
    runPure =
      run
        . runStubRender
        . runStubVideoImport
        . runStubAudioImport
        . runStubLogger
        . runInMemoryProjectStore
        . runStubUserInterface (eventsVector (cmds <> pure Exit))
        . runTimelineMode

currentTimeline :: Lens' TimelineState (Timeline ())
currentTimeline = history.UndoRedo.current.existingProject.project.timeline

showTimelineAndFocus (t, f) = drawTree (timelineToTree t) <> "\n" <> show f

hprop_undo_actions_are_undoable = property $ do
  timelineAndFocus <- forAllWith showTimelineAndFocus (Gen.timelineWithFocus (Range.linear 0 10) Gen.parallel)
  initialState <- forAll (initializeState timelineAndFocus)
  cmds <- forAllWith (show . map commandName)
                     (Gen.list (Range.linear 1 10) genUndoableTimelineCommand)

  -- we begin by running 'cmds' on the original state
  beforeUndos <- runTimelineStubbedWithExit cmds initialState
  annotate (drawTree (timelineToTree (beforeUndos^.currentTimeline)))

  -- then we run as many undo commands as undoable commands
  afterUndos <- runTimelineStubbedWithExit (Undo <$ cmds) beforeUndos
  timelineToTree (initialState ^. currentTimeline) === timelineToTree (afterUndos ^. currentTimeline)

hprop_undo_actions_are_redoable = property $ do
  timelineAndFocus <- forAllWith showTimelineAndFocus (Gen.timelineWithFocus (Range.linear 0 10) Gen.parallel)
  initialState <- forAll (initializeState timelineAndFocus)
  cmds <- forAllWith (show . map commandName)
                     (Gen.list (Range.linear 1 10) genUndoableTimelineCommand)
  -- we begin by running 'cmds' on the original state
  beforeUndos <- runTimelineStubbedWithExit cmds initialState
  -- then we undo and redo all of them
  afterRedos  <-
    runTimelineStubbedWithExit (Undo <$ cmds) beforeUndos
    >>= runTimelineStubbedWithExit (Redo <$ cmds)
  -- that should result in a timeline equal to the one we had before
  -- starting the undos
  timelineToTree (beforeUndos ^. currentTimeline) === timelineToTree (afterRedos ^. currentTimeline)
