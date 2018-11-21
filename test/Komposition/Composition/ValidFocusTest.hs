{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

module Komposition.Composition.ValidFocusTest where

import           Komposition.Prelude
import qualified Prelude

import           Control.Lens
import           Hedgehog                           hiding (Parallel)
import qualified Hedgehog.Gen                       as Gen hiding (parallel)
import           Hedgehog.Range

import           Komposition.Composition
import           Komposition.Composition.Delete
import           Komposition.Composition.Insert
import           Komposition.Composition.Split
import           Komposition.Focus
import           Komposition.Focus.Parent
import           Komposition.History
import           Komposition.Library
import           Komposition.MediaType
import           Komposition.Project
import           Komposition.VideoSettings

import qualified Komposition.Composition.Generators as Gen

data TestCommand
  = TestChangeFocus FocusCommand
  | TestInsert (Insertion ()) InsertPosition
  | TestSplit
  | TestDelete
  | TestUndo
  | TestRedo
  deriving (Eq, Show)

currentProject p = current (p ^. projectHistory)

changeFocusCommand
  :: MonadGen m
  => ExistingProject
  -> Focus (ToFocusType Timeline)
  -> m TestCommand
changeFocusCommand p focus =
  case atFocus focus (currentProject p ^. timeline) of
    Just FocusedSequence{} ->
      Gen.element (TestChangeFocus <$> [FocusDown, FocusLeft, FocusRight])
    Just FocusedParallel{}  -> TestChangeFocus <$> Gen.enumBounded
    Just FocusedVideoPart{} -> Gen.element
      (TestChangeFocus <$> [FocusUp, FocusDown, FocusLeft, FocusRight])
    Just FocusedAudioPart{} ->
      Gen.element (TestChangeFocus <$> [FocusUp, FocusLeft, FocusRight])
    Nothing -> Gen.discard

insertCommand
  :: MonadGen m
  => ExistingProject
  -> Focus (ToFocusType Timeline)
  -> m TestCommand
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

splitCommands
  :: MonadGen m
  => ExistingProject
  -> Focus (ToFocusType Timeline)
  -> [m TestCommand]
splitCommands p focus =
  case (parentAtFocus focus (currentProject p ^. timeline), focus) of
    (Just (SequenceParent (Sequence _ pars)), SequenceFocus _ (Just (ParallelFocus pIdx Nothing)))
      | pars `validToSplitAt` pIdx
      -> [pure TestSplit]
    (Just (ParallelParent (Parallel _ vs as)), SequenceFocus _ (Just (ParallelFocus _ (Just (ClipFocus mt cIdx)))))
      | mt == Video && vs `validToSplitAt` cIdx
      -> [pure TestSplit]
      | mt == Audio && as `validToSplitAt` cIdx
      -> [pure TestSplit]
    _ -> []
  where
    validToSplitAt xs idx = length xs >= 2 && idx > 0 && idx < (length xs - 1)

deleteCommands
  :: MonadGen m
  => ExistingProject
  -> Focus (ToFocusType Timeline)
  -> [m TestCommand]
deleteCommands p focus =
  case parentAtFocus focus (currentProject p ^. timeline) of
    Just (TimelineParent (Timeline seqs)) | length seqs >= 2 ->
      [pure TestDelete]
    Just (SequenceParent (Sequence _ pars)) | length pars >= 2 ->
      [pure TestDelete]
    Just (ParallelParent _) -> [pure TestDelete]
    _                       -> []

testCommand
  :: MonadGen m
  => ExistingProject
  -> Focus (ToFocusType Timeline)
  -> m TestCommand
testCommand composition focus = Gen.frequency
  (  [(20, insertCommand composition focus)]
  <> map (10, ) (splitCommands composition focus)
  <> map (10, ) (deleteCommands composition focus)
  <> [(1, pure TestUndo), (1, pure TestRedo)]
  )

applyTestCommand
  :: Monad m
  => Focus SequenceFocusType
  -> ExistingProject
  -> TestCommand
  -> PropertyT m (ExistingProject, Focus SequenceFocusType)
applyTestCommand focus ep = \case
  TestChangeFocus cmd ->
    modifyTimeline $ \tl -> case modifyFocus tl cmd focus of
    -- We ignore out of bounds movements as the generator isn't smart enough yet.
      Left  OutOfBounds -> pure (tl, focus)
      -- Other movement errors are considered failures.
      Left  e           -> footnoteShow e >> failure
      Right focus'      -> pure (tl, focus')
  TestInsert insertion position -> modifyTimeline $ \tl ->
    maybe failure (pure . (, focus)) (insert focus insertion position tl)
  TestSplit -> modifyTimeline $ \tl -> maybe failure pure (split focus tl)
  TestDelete ->
    modifyTimeline $ \tl -> handleDeleteResult focus tl (delete focus tl)
  TestUndo -> pure (ep & projectHistory %%~ undo & fromMaybe ep, focus)
  TestRedo -> pure (ep & projectHistory %%~ undo & fromMaybe ep, focus)
  where
    modifyTimeline f = do
      (tl, focus') <- f (currentProject ep ^. timeline)
      return (ep & projectHistory %~ edit (set timeline tl), focus')
    handleDeleteResult initialFocus tl = \case
      Nothing              -> failure
      Just (tl', Just cmd) -> do
        focus' <- either (\e -> footnoteShow e >> failure)
                         pure
                         (modifyFocus tl cmd initialFocus)
        pure (tl', focus')
      Just (tl', Nothing) -> pure (tl', initialFocus)

generateAndApplyTestCommands
  :: Monad m
  => ExistingProject
  -> Focus SequenceFocusType
  -> Int
  -> PropertyT m (ExistingProject, Focus SequenceFocusType)
generateAndApplyTestCommands ep focus n
  | n == 0 = pure (ep, focus)
  | otherwise = do
    cmd           <- forAll (testCommand ep focus)
    (ep', focus') <- applyTestCommand focus ep cmd
    generateAndApplyTestCommands ep' focus' (pred n)

initialProject :: Timeline () -> Project
initialProject tl = Project
  { _projectName   = "Test"
  , _timeline      = tl
  , _library       = Library [] []
  , _videoSettings = AllVideoSettings
    { _renderVideoSettings = VideoSettings
      { _frameRate  = 25
      , _resolution = Resolution 1920 1080
      }
    , _proxyVideoSettings  = VideoSettings
      { _frameRate  = 25
      , _resolution = Resolution 960 540
      }
    }
  }

hprop_focusNeverGoesInvalid = withTests 1000 $ property $ do
  (tl, focus, n) <- forAll $ do
    (tl, focus) <- Gen.timelineWithFocus (linear 0 3) Gen.parallel
    n           <- Gen.integral (linear 1 10)
    pure (tl, focus, n)
  let ep = ExistingProject (ProjectPath "foo") (initialise (initialProject tl))
  (ep', focus') <- generateAndApplyTestCommands ep focus n
  assert . isJust $ atFocus focus' (currentProject ep' ^. timeline)

{-# ANN module ("HLint: ignore Use camelCase" :: Prelude.String) #-}
