{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TupleSections         #-}
module Komposition.UndoRedoTest where

import           Komposition.Prelude  hiding (applyN)

import           Hedgehog             hiding (Parallel)
import qualified Hedgehog.Gen         as Gen hiding (parallel)
import qualified Hedgehog.Range       as Range

import           Komposition.UndoRedo

data TestAction = SetValue Int Int
  deriving (Eq, Show)

instance Invertible TestAction where
  invert (SetValue old new) = SetValue new old

instance Applicative m => Runnable TestAction Int m where
  run (SetValue _ new) _ = pure new

genTestActions :: MonadGen m => Int -> m [Directed Forward TestAction]
genTestActions initialOld = do
  len <- Gen.int (Range.linear 0 100)
  genActions initialOld len []
  where
    genActions _ 0 actions = pure actions
    genActions old len actions = do
      new <- Gen.filter (/= old) (Gen.int (Range.linear (-100) 100))
      genActions new (pred len) (actions <> [Directed (SetValue old new)])

runAndRecordAll
  :: (Monad m, Invertible action, Runnable action state m)
  => History action state
  -> [Directed Forward action]
  -> m (History action state)
runAndRecordAll = foldM (flip runAndRecord)

applyN
  :: (Monad m, MonadTest m, Invertible action, Runnable action state m)
  => (History action state -> Maybe (m (History action state)))
  -> (History action state)
  -> Int
  -> m (History action state)
applyN f acc n = foldM
  (\history' _ -> case f history' of
    Just ma -> ma
    Nothing -> failure
  )
  acc
  (replicate n ())

applyAll
  :: (Monad m, MonadTest m, Invertible action, Runnable action state m)
  => (History action state -> Maybe (m (History action state)))
  -> History action state
  -> m (History action state)
applyAll f history =
  case f history of
    Just ma -> applyAll f =<< ma
    Nothing -> pure history

hprop_undo_history_has_correct_number_of_undos = property $ do
  initial <- forAll (Gen.int Range.linearBounded)
  actions <- forAllWith (show . map unDirected) (genTestActions initial)
  history <- runAndRecordAll (init initial) actions
  numUndos history === length actions

hprop_undo_all_returns_initial_state = property $ do
  initial <- forAll (Gen.int Range.linearBounded)
  actions <- forAllWith (show . map unDirected) (genTestActions initial)
  -- we apply all actions
  afterActions <- runAndRecordAll (init initial) actions
  -- then undo them all
  numUndos afterActions === length actions
  afterUndos <- applyAll undo afterActions
  -- and we expect to be back at the initial state
  current afterUndos === initial

hprop_redo_all_returns_final_state = property $ do
  initial <- forAll (Gen.int Range.linearBounded)
  actions <- forAllWith (show . map unDirected) (genTestActions initial)
  -- we apply all actions
  afterActions <- runAndRecordAll (init initial) actions
  -- then undo and redo them all
  afterRedos <- applyAll undo afterActions >>= applyAll redo
  -- and we expect to be back at the initial state
  current afterRedos === current afterActions
