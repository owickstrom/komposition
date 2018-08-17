{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
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
import qualified Hedgehog.Gen                  as Gen
                                                   hiding ( parallel )

import           FastCut.Composition
import           FastCut.Composition.Insert
import           FastCut.Composition.Delete
import           FastCut.Focus.Parent
import           FastCut.Focus

import qualified FastCut.Composition.Generators
                                               as Gen

data TestCommand
  = TestChangeFocus FocusCommand
  | TestInsert (Insertion ()) InsertPosition
  | TestDelete
  deriving (Eq, Show)

changeFocusCommand
  :: MonadGen m => Composition () TimelineType -> Focus ft -> m TestCommand
changeFocusCommand composition focus = case atFocus focus composition of
  Just FocusedSequence{} ->
    Gen.element (TestChangeFocus <$> [FocusDown, FocusLeft, FocusRight])
  Just FocusedParallel{}  -> TestChangeFocus <$> Gen.enumBounded
  Just FocusedVideoPart{} -> Gen.element
    (TestChangeFocus <$> [FocusUp, FocusDown, FocusLeft, FocusRight])
  Just FocusedAudioPart{} ->
    Gen.element (TestChangeFocus <$> [FocusUp, FocusLeft, FocusRight])
  Nothing -> Gen.discard

insertCommand
  :: MonadGen m => Composition () TimelineType -> Focus ft -> m TestCommand
insertCommand _ focus = case focusType focus of
  SequenceFocusType ->
    TestInsert
      <$> (InsertSequence <$> Gen.sequence' (linear 1 5) Gen.parallel)
      <*> Gen.enumBounded
  ParallelFocusType ->
    TestInsert
      <$> Gen.choice
            [ InsertParallel <$> Gen.parallel
            , InsertVideoPart <$> Gen.videoPart
            , InsertAudioPart <$> Gen.audioPart
            ]
      <*> Gen.enumBounded
  ClipFocusType ->
    TestInsert
      <$> Gen.choice
            [ InsertVideoPart <$> Gen.videoPart
            , InsertAudioPart <$> Gen.audioPart
            ]
      <*> Gen.enumBounded

deleteCommands
  :: MonadGen m => Composition () TimelineType -> Focus ft -> [m TestCommand]
deleteCommands composition focus = case parentAtFocus focus composition of
  Just (TimelineParent (Timeline _ seqs)) | length seqs >= 2 ->
    [pure TestDelete]
  Just (SequenceParent (Sequence _ pars)) | length pars >= 2 ->
    [pure TestDelete]
  Just (ParallelParent _) -> [pure TestDelete]
  _                       -> []

testCommand
  :: MonadGen m => Composition () TimelineType -> Focus ft -> m TestCommand
testCommand composition focus = Gen.frequency
  (  [(2, insertCommand composition focus)]
  <> map (1, ) (deleteCommands composition focus)
  )

applyTestCommand
  :: Monad m
  => TestCommand
  -> Composition () TimelineType
  -> Focus SequenceFocusType
  -> PropertyT
       m
       (Composition () TimelineType, Focus SequenceFocusType)
applyTestCommand = \case
  TestChangeFocus cmd -> \comp focus -> case modifyFocus comp cmd focus of
    -- We ignore out of bounds movements as the generator isn't smart enough yet.
    Left  OutOfBounds -> pure (comp, focus)
    -- Other movement errors are considered failures.
    Left  e           -> footnoteShow e >> failure
    Right focus'      -> pure (comp, focus')
  TestInsert insertion position -> \comp focus ->
    maybe failure (pure . (, focus)) (insert focus insertion position comp)
  TestDelete -> \comp focus -> case delete focus comp of
    Nothing                -> failure
    Just (comp', Just cmd) -> do
      focus' <- either (\e -> footnoteShow e >> failure)
                       pure
                       (modifyFocus comp cmd focus)
      pure (comp', focus')
    Just (comp', Nothing) -> pure (comp', focus)

generateAndApplyTestCommands
  :: Monad m
  => Composition () TimelineType
  -> Focus SequenceFocusType
  -> Int
  -> PropertyT
       m
       (Composition () TimelineType, Focus SequenceFocusType)
generateAndApplyTestCommands timeline focus n
  | n == 0 = pure (timeline, focus)
  | otherwise = do
    cmd <- forAll (testCommand timeline focus)
    applyTestCommand cmd timeline focus

hprop_focusNeverGoesInvalid = withTests 1000 $ property $ do
  (timeline, focus, n) <- forAll $ do
    (timeline, focus) <- Gen.timelineWithFocus (linear 0 3) Gen.parallel
    n                 <- Gen.integral (linear 1 10)
    pure (timeline, focus, n)
  (timeline', focus') <- generateAndApplyTestCommands timeline focus n
  assert . isJust $ atFocus focus' timeline'

{-# ANN module ("HLint: ignore Use camelCase" :: Prelude.String) #-}
