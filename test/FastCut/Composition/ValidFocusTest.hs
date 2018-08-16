{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
{-# OPTIONS_GHC -fno-warn-unticked-promoted-constructors #-}

module FastCut.Composition.ValidFocusTest where

import           FastCut.Prelude
import qualified Prelude

import           Hedgehog
import           Hedgehog.Range
import qualified Hedgehog.Gen                  as Gen hiding (parallel)

import           FastCut.Composition
import           FastCut.Composition.Insert
import           FastCut.Composition.Delete
import           FastCut.Focus

import qualified FastCut.Composition.Generators
                                               as Gen

data TestCommand
  = TestChangeFocus FocusCommand
  | TestInsert (Insertion ()) InsertPosition
  | TestDelete
  deriving (Eq, Show)

testCommand :: MonadGen m => m TestCommand
testCommand = Gen.choice
  [ TestChangeFocus <$> Gen.enumBounded
  , TestInsert
    <$> Gen.choice [ InsertSequence <$> Gen.sequence' (linear 1 5)
                   , InsertParallel <$> Gen.parallel (linear 1 5)
                   , InsertVideoPart <$> Gen.videoPart (linear 1 5)
                   , InsertAudioPart <$> Gen.audioPart (linear 1 5)
                   ]
    <*> Gen.enumBounded
  , pure TestDelete
  ]

insertionMatchesFocus :: Insertion () -> Focus ft -> Bool
insertionMatchesFocus insertion focus =
  case (insertion, focusType focus) of
    (InsertSequence{}, SequenceFocusType) -> True
    (InsertParallel{}, ParallelFocusType) -> True
    (InsertVideoPart{}, ClipFocusType) -> True
    (InsertAudioPart{}, ClipFocusType) -> True
    _ -> False

applyTestCommand
  :: Monad m
  => TestCommand
  -> Composition () TimelineType
  -> Focus SequenceFocusType
  -> PropertyT m (Composition () TimelineType, Focus SequenceFocusType)
applyTestCommand = \case
  TestChangeFocus cmd -> \comp focus -> case modifyFocus comp cmd focus of
    Left UnhandledFocusModification{} -> failure
    Left  _      -> pure (comp, focus)
    Right focus' -> pure (comp, focus')
  TestInsert insertion position ->
    \comp focus -> do
      -- We're only interested in insertions that match the current focus.
      when (not (insertionMatchesFocus insertion focus))
        discard
      maybe failure (pure . (,focus))  (insert focus insertion position comp)
  TestDelete -> \comp focus -> either (const failure) pure (delete_ focus comp)

applyTestCommands
  :: Monad m
  => Composition () TimelineType
  -> Focus SequenceFocusType
  -> [TestCommand]
  -> PropertyT m (Composition () TimelineType, Focus SequenceFocusType)
applyTestCommands timeline focus =
  foldM (\(tl, f) cmd -> applyTestCommand cmd tl f) (timeline, focus)

hprop_focusNeverGoesInvalid = withDiscards 1000 $ property $ do
  (timeline, focus, commands) <- forAll $ do
    (timeline, focus) <- Gen.timelineWithFocus (linear 1 10)
    commands          <- Gen.list (linear 1 10) testCommand
    pure (timeline, focus, commands)
  (timeline', focus') <- applyTestCommands timeline focus commands
  assert . isJust $ atFocus focus' timeline'

{-# ANN module ("HLint: ignore Use camelCase" :: Prelude.String) #-}
