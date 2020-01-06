{-# OPTIONS_GHC -fno-warn-missing-signatures -fno-warn-partial-type-signatures #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE OverloadedLabels      #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE TypeFamilies          #-}
module Komposition.Application.TimelineModeTest where

import           Komposition.Prelude

import           Control.Effect
import           Control.Lens
import           Data.Row.Records                              (Empty)
import           Data.Tree                                     (drawTree)
import qualified Data.Vector                                   as Vector
import           Hedgehog                                      hiding (Command)
import qualified Hedgehog.Gen                                  as Gen hiding
                                                                       (parallel)
import qualified Hedgehog.Range                                as Range
import           Motor.FSM                                     (ireturn, (>>>),
                                                                (>>>=))

import           Komposition.Application.Base                  (Application)
import           Komposition.Application.KeyMaps
import           Komposition.Application.TimelineMode
import           Komposition.Composition                       (Timeline)
import           Komposition.Focus
import           Komposition.MediaType
import           Komposition.Project
import qualified Komposition.UndoRedo                          as UndoRedo
import           Komposition.UserInterface                     hiding (TimelineViewModel (..),
                                                                project)
import           Komposition.UserInterface.WindowUserInterface

import           Komposition.Browser.Stub                      (runStubBrowser)
import qualified Komposition.Composition.Generators            as Gen
import           Komposition.Composition.ToTree
import           Komposition.Import.Audio.StubAudioImport
import           Komposition.Import.Video.StubVideoImport
import           Komposition.Logging.StubLogger
import qualified Komposition.Project.Generators                as Gen
import           Komposition.Project.InMemoryProjectStore
import           Komposition.Render.StubRender
import           Komposition.UserInterface.StubUserInterface


initializeState
  :: (MonadGen m, GenBase m ~ Identity)
  => (Timeline (), Focus 'SequenceFocusType)
  -> m (TimelineState _ _)
initializeState (timeline', focus') = do
  existingProject' <-
    ExistingProject
    <$> (ProjectPath <$> Gen.string (Range.linear 1 10) Gen.unicode)
    <*> Gen.projectWithTimelineAndFocus (pure (timeline', focus'))
  pure TimelineState { _existingProject = initializeHistory existingProject'
                     , _statusMessage   = Nothing
                     , _clipboard       = Nothing
                     , _zoomLevel       = ZoomLevel 1
                     , _preview         = NotPreviewing
                     }

genUndoableTimelineEvent :: (MonadGen m, GenBase m ~ Identity) => m SomeEvent
genUndoableTimelineEvent =
  -- TODO: add Insert
  SomeEvent <$>
  Gen.choice
  [ pure (CommandKeyMappedEvent Delete)
  , CommandKeyMappedEvent . Paste <$> Gen.enumBounded
  , FocusedClipSpeedSet <$> Gen.genVideoSpeed
  , FocusedClipStartSet <$> Gen.duration' (Range.linearFrac 0 10)
  , FocusedClipEndSet <$> Gen.duration' (Range.linearFrac 10 20)
  , pure (CommandKeyMappedEvent Split)
  , pure (CommandKeyMappedEvent Join)
  ]

undoEvent = SomeEvent (CommandKeyMappedEvent Undo)
redoEvent = SomeEvent (CommandKeyMappedEvent Redo)

runTimelineMode
  :: (Application t m sig, TimelineEffects sig)
  => TimelineState t m
  -> t m Empty Empty (TimelineModeResult t m)
runTimelineMode state' =
  newWindow #gui (timelineViewFromState state') keymap
    >>> timelineMode #gui state'
    >>>= \r -> destroyWindow #gui >>> ireturn r
  where keymap = CommandKeyMappedEvent <$> keymaps STimelineMode

runTimelineStubbedWithExit
  :: MonadTest m
  => [SomeEvent]
  -> TimelineState StubUserInterface _
  -> m (TimelineState StubUserInterface _)
runTimelineStubbedWithExit events state' = case runPure state' of
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
        . runStubBrowser
        . runStubUserInterface (Vector.fromList (events <> pure (SomeEvent (CommandKeyMappedEvent Exit))))
        . runTimelineMode

currentTimeline :: Getter (TimelineState t m) (Timeline ())
currentTimeline = existingProject.project.timeline.UndoRedo.current

showTimelineAndFocus (t, f) = drawTree (timelineToTree t) <> "\n" <> show f

hprop_undo_actions_are_undoable = property $ do
  timelineAndFocus <- forAllWith showTimelineAndFocus (Gen.timelineWithFocus (Range.linear 0 10) Gen.parallel)
  initialState <- forAll (initializeState timelineAndFocus)
  events <- forAll (Gen.list (Range.exponential 1 100) genUndoableTimelineEvent)
  -- we begin by running 'events' on the original state
  beforeUndos <- runTimelineStubbedWithExit events initialState
  annotate (drawTree (timelineToTree (beforeUndos^.currentTimeline)))
  -- then we run as many undo commands as undoable commands
  afterUndos <- runTimelineStubbedWithExit (undoEvent <$ events) beforeUndos
  -- that should result in a timeline equal to the one we at the
  -- beginning
  timelineToTree (initialState ^. currentTimeline) === timelineToTree (afterUndos ^. currentTimeline)

hprop_undo_actions_are_redoable = property $ do
  timelineAndFocus <- forAllWith showTimelineAndFocus (Gen.timelineWithFocus (Range.linear 0 10) Gen.parallel)
  initialState <- forAll (initializeState timelineAndFocus)
  events <- forAll (Gen.list (Range.exponential 1 100) genUndoableTimelineEvent)
  -- we begin by running 'events' on the original state
  beforeUndos <- runTimelineStubbedWithExit events initialState
  -- then we undo and redo all of them
  afterRedos  <-
    runTimelineStubbedWithExit (undoEvent <$ events) beforeUndos
    >>= runTimelineStubbedWithExit (redoEvent <$ events)
  -- that should result in a timeline equal to the one we had before
  -- starting the undos
  timelineToTree (beforeUndos ^. currentTimeline) === timelineToTree (afterRedos ^. currentTimeline)

genMediaType :: (MonadGen m, GenBase m ~ Identity) => m MediaType
genMediaType = Gen.choice [pure Video, pure Audio]

genInsertType :: (MonadGen m, GenBase m ~ Identity) => m InsertType
genInsertType =
  Gen.choice
  [ pure InsertComposition
  , InsertClip <$> Gen.maybe genMediaType
  , InsertGap <$> Gen.maybe genMediaType
  ]

genFocusChangingEvents :: (MonadGen m, GenBase m ~ Identity) => m [SomeEvent]
genFocusChangingEvents = Gen.choice
  [pure <$> genUndoableTimelineEvent, pure <$> genFocusEvent, genInsertEvents]
  where
    genFocusEvent =
      SomeEvent . CommandKeyMappedEvent . FocusCommand <$> Gen.enumBounded
    genInsertEvents = do
      insert        <- InsertCommand <$> genInsertType <*> Gen.enumBounded
      libraryEvents <- Gen.list (Range.linear 0 20) $ Gen.choice
        [ pure (LibraryAssetsSelected SVideo [])
        , pure (LibraryAssetsSelected SAudio [])
        ]
      exitEvent <- Gen.choice
        [ pure LibrarySelectionConfirmed
        , pure (CommandKeyMappedEvent Cancel)
        , pure WindowClosed
        ]
      pure
        (  [SomeEvent (CommandKeyMappedEvent insert)]
        <> map SomeEvent libraryEvents
        <> [SomeEvent exitEvent]
        )

hprop_focus_never_goes_invalid = property $ do
  timelineAndFocus <- forAllWith
    showTimelineAndFocus
    (Gen.timelineWithFocus (Range.linear 0 10) Gen.parallel)
  initialState <- forAll (initializeState timelineAndFocus)
  events <- forAll (Gen.list (Range.exponential 1 500) genFocusChangingEvents)
  endState <- runTimelineStubbedWithExit (concat events) initialState
  assert . isJust $ atFocus
    (endState ^. existingProject . project . timelineFocus)
    (endState ^. existingProject . project . timeline . UndoRedo.current)
