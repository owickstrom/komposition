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
import           Komposition.Project
import           Komposition.UserInterface

import qualified Komposition.Composition.Generators          as Gen
import           Komposition.Import.Audio.StubAudioImport
import           Komposition.Import.Video.StubVideoImport
import           Komposition.Logging.StubLogger
import qualified Komposition.Project.Generators              as Gen
import           Komposition.Project.InMemoryProjectStore
import           Komposition.Render.StubRender
import           Komposition.UserInterface.StubUserInterface


genTimelineModel = do
  (timeline', focus') <- Gen.timelineWithFocus (Range.linear 0 10) Gen.parallel
  existingProject'    <-
    ExistingProject
    <$> (ProjectPath <$> Gen.string (Range.linear 1 10) Gen.unicode)
    <*> Gen.projectWithTimeline (pure timeline')
  pure TimelineModel
    { _existingProject  = existingProject'
    , _currentFocus     = focus'
    , _statusMessage    = Nothing
    , _clipboard        = Nothing
    , _zoomLevel        = ZoomLevel 1
    , _previewImagePath = Nothing
    }

genUndoableTimelineCommand :: MonadGen m => m (Command 'TimelineMode)
genUndoableTimelineCommand =
  Gen.choice [pure Split, pure Delete, Paste <$> Gen.enumBounded]

eventsVector =
  Vector.fromList . map (SomeEvent . CommandKeyMappedEvent)

runTimelineMode
  :: (Application t m sig, TimelineEffects sig) => TimelineModel -> t m Empty Empty TimelineModeResult
runTimelineMode model =
  newWindow #gui (timelineView model) keymap
    >>> timelineMode #gui model
    >>>= \r -> destroyWindow #gui >>> ireturn r
  where keymap = CommandKeyMappedEvent <$> keymaps STimelineMode

runTimelineStubbedWithExit
  :: MonadTest m
  => [Command 'TimelineMode]
  -> TimelineModel
  -> m TimelineModel
runTimelineStubbedWithExit cmds model = case runPure model of
  Left  err                     -> annotateShow err >> failure
  Right TimelineClose           -> failure
  Right (TimelineExit endModel) -> pure endModel
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

hasEqualTimelineTo m1 m2 =
  m1 ^. existingProject . project . timeline
  ===
  m2 ^. existingProject . project . timeline

hprop_undo_actions_are_inversible = property $ do
  initialModel <- forAll genTimelineModel
  cmds <- forAllWith (show . map commandName)
                     (Gen.list (Range.linear 1 10) genUndoableTimelineCommand)
  let undos = Undo <$ cmds
  -- we run as many undo commands as undoable commands
  afterUndos <- runTimelineStubbedWithExit (cmds <> undos) initialModel
  initialModel `hasEqualTimelineTo` afterUndos

hprop_undo_actions_are_redoable = property $ do
  initialModel <- forAll genTimelineModel
  cmds <- forAllWith (show . map commandName)
                     (Gen.list (Range.linear 1 10) genUndoableTimelineCommand)
  -- we begin by running 'cmds' on the original model
  beforeUndos <- runTimelineStubbedWithExit cmds initialModel
  -- then we undo and redo all of them
  afterRedos  <-
    runTimelineStubbedWithExit (Undo <$ cmds) beforeUndos
    >>= runTimelineStubbedWithExit (Redo <$ cmds)
  -- that should result in a timeline equal to the one we had before
  -- starting the undos
  beforeUndos `hasEqualTimelineTo` afterRedos
