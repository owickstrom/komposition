{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

module FastCut.Composition.ValidFocusTest where

import           FastCut.Prelude
import qualified Prelude

import           Hedgehog                       hiding (Parallel)
import qualified Hedgehog.Gen                   as Gen hiding (parallel)
import           Hedgehog.Range

import           FastCut.Composition
import           FastCut.Composition.Delete
import           FastCut.Composition.Insert
import           FastCut.Composition.Split
import           FastCut.Focus
import           FastCut.Focus.Parent
import           FastCut.MediaType

import qualified FastCut.Composition.Generators as Gen

data TestCommand
  = TestChangeFocus FocusCommand
  | TestInsert (Insertion ()) InsertPosition
  | TestSplit
  | TestDelete
  deriving (Eq, Show)

changeFocusCommand
  :: MonadGen m => Timeline () -> Focus (ToFocusType Timeline) -> m TestCommand
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
  :: MonadGen m => Timeline () -> Focus (ToFocusType Timeline) -> m TestCommand
insertCommand _ focus = case focusType focus of
  SequenceFocusType ->
    TestInsert
      <$> (InsertSequence <$> Gen.sequence' (linear 1 5) Gen.parallel)
      <*> Gen.enumBounded
  ParallelFocusType ->
    TestInsert
      <$> Gen.choice
            [ InsertParallel <$> Gen.parallel
            , InsertVideoParts <$> Gen.list (linear 0 5) Gen.videoPart
            , InsertAudioParts <$> Gen.list (linear 0 5) Gen.audioPart
            ]
      <*> Gen.enumBounded
  ClipFocusType ->
    TestInsert
      <$> Gen.choice
            [ InsertVideoParts <$> Gen.list (linear 0 5) Gen.videoPart
            , InsertAudioParts <$> Gen.list (linear 0 5) Gen.audioPart
            ]
      <*> Gen.enumBounded

splitCommands :: MonadGen m => Timeline () -> Focus (ToFocusType Timeline) -> [m TestCommand]
splitCommands timeline focus =
  case (parentAtFocus focus timeline, focus) of
    (Just (SequenceParent (Sequence _ pars)), SequenceFocus _ (Just (ParallelFocus pIdx Nothing)))
      | pars `validToSplitAt` pIdx -> [pure TestSplit]
    (Just (ParallelParent (Parallel _ vs as)), SequenceFocus _ (Just (ParallelFocus _ (Just (ClipFocus mt cIdx)))))
      | mt == Video && vs `validToSplitAt` cIdx -> [pure TestSplit]
      | mt == Audio && as `validToSplitAt` cIdx -> [pure TestSplit]
    _ -> []
  where
    validToSplitAt xs idx =
      length xs >= 2 && idx > 0 && idx < (length xs - 1)

deleteCommands
  :: MonadGen m => Timeline () -> Focus (ToFocusType Timeline) -> [m TestCommand]
deleteCommands composition focus = case parentAtFocus focus composition of
  Just (TimelineParent (Timeline seqs)) | length seqs >= 2 ->
    [pure TestDelete]
  Just (SequenceParent (Sequence _ pars)) | length pars >= 2 ->
    [pure TestDelete]
  Just (ParallelParent _) -> [pure TestDelete]
  _                       -> []

testCommand
  :: MonadGen m => Timeline () -> Focus (ToFocusType Timeline) -> m TestCommand
testCommand composition focus = Gen.frequency
  (  [(2, insertCommand composition focus)]
  <> map (1, ) (splitCommands composition focus)
  <> map (1, ) (deleteCommands composition focus)
  )

applyTestCommand
  :: Monad m
  => TestCommand
  -> Timeline ()
  -> Focus SequenceFocusType
  -> PropertyT
       m
       (Timeline (), Focus SequenceFocusType)
applyTestCommand = \case
  TestChangeFocus cmd -> \comp focus -> case modifyFocus comp cmd focus of
    -- We ignore out of bounds movements as the generator isn't smart enough yet.
    Left  OutOfBounds -> pure (comp, focus)
    -- Other movement errors are considered failures.
    Left  e           -> footnoteShow e >> failure
    Right focus'      -> pure (comp, focus')
  TestInsert insertion position -> \comp focus ->
    maybe failure (pure . (, focus)) (insert focus insertion position comp)
  TestSplit -> \comp focus ->
    maybe failure pure (split focus comp)
  TestDelete -> \comp focus ->
    handleDeleteResult focus comp (delete focus comp)
  where
    handleDeleteResult focus comp = \case
      Nothing                -> failure
      Just (comp', Just cmd) -> do
        focus' <- either (\e -> footnoteShow e >> failure)
                         pure
                         (modifyFocus comp cmd focus)
        pure (comp', focus')
      Just (comp', Nothing) -> pure (comp', focus)

generateAndApplyTestCommands ::
     Monad m
  => Timeline ()
  -> Focus SequenceFocusType
  -> Int
  -> PropertyT m (Timeline (), Focus SequenceFocusType)
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
