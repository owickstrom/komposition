{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}

{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module Komposition.UndoRedoTest where

import           Komposition.Prelude  hiding (applyN)

import           Control.Lens
import           Data.Vector          (Vector, (!), (//))
import qualified Data.Vector          as Vector
import           Hedgehog             hiding (Parallel)
import qualified Hedgehog.Gen         as Gen hiding (parallel)
import qualified Hedgehog.Range       as Range

import           Komposition.UndoRedo

type Idx = Int
type Value = Char

data TestAction
  = SetValue Idx Char
  | UnsetValue Idx Char
  deriving (Eq, Show)

instance Applicative m => Runnable TestAction (Vector Char) () m where
  run (SetValue idx new) values = pure ((UnsetValue idx (values ! idx), values // [(idx, new)]), ())
  run (UnsetValue idx old) values = pure ((SetValue idx (values ! idx), values // [(idx, old)]), ())

genTestActions :: MonadGen m => Range Int -> Int -> m [TestAction]
genTestActions range numValues = Gen.list
  range
  (SetValue <$> Gen.integral (Range.linear 0 (pred numValues)) <*> Gen.ascii)

genTestValues :: MonadGen m => Range Int -> m (Vector Char)
genTestValues range = do
  n <- Gen.integral range
  pure ( Vector.replicate n '\NUL')

runAndRecordAll
  :: (Monad m, Runnable action state () m)
  => History action state
  -> [action]
  -> m (History action state)
runAndRecordAll = foldM (\history action -> runAndRecord action history <&>
                        \(newHistory, ()) -> newHistory)

applyN
  :: (Monad m, MonadTest m, Runnable action state () m)
  => (History action state -> Maybe (m (History action state)))
  -> History action state
  -> Int
  -> m (History action state)
applyN f acc n = foldM
  (\history' _ -> fromMaybe failure (f history'))
  acc
  (replicate n ())

applyAll
  :: (Monad m, MonadTest m, Runnable action state () m)
  => (History action state -> Maybe (m (History action state, ())))
  -> History action state
  -> m (History action state)
applyAll f history =
  case f history of
    Just ma -> applyAll f . fst =<< ma
    Nothing -> pure history

genTestValuesAndActions :: MonadGen m => m (Vector Char, [TestAction])
genTestValuesAndActions = do
  initial <- genTestValues (Range.exponential 1 100)
  actions <- genTestActions (Range.exponential 0 100) (Vector.length initial)
  pure (initial, actions)

hprop_undo_history_has_correct_number_of_undos = property $ do
  (initial, actions) <- forAll genTestValuesAndActions
  history <- runAndRecordAll (init initial) actions
  numUndos history === length actions

hprop_undo_all_returns_initial_state = property $ do
  (initial, actions) <- forAll genTestValuesAndActions
  -- we apply all actions
  afterActions <- runAndRecordAll (init initial) actions
  -- then undo them all
  numUndos afterActions === length actions
  afterUndos <- applyAll undo afterActions
  -- and we expect to be back at the initial state
  afterUndos ^. current === initial

hprop_redo_all_returns_final_state = property $ do
  (initial, actions) <- forAll genTestValuesAndActions
  -- we apply all actions
  afterActions <- runAndRecordAll (init initial) actions
  -- then undo and redo them all
  afterRedos <- applyAll undo afterActions >>= applyAll redo
  -- and we expect to be back at the initial state
  afterRedos ^. current === afterActions ^. current

hprop_run_new_action_clears_redos = property $ do
  (initial, actions) <- forAll genTestValuesAndActions
  -- we apply all actions
  afterActions <- runAndRecordAll (init initial) actions
  -- and undo them all
  afterUndos   <- applyAll undo afterActions
  -- make sure we have the redos
  numRedos afterUndos === length actions

  -- then generate more actions to run, at least one
  moreActions <- forAll (genTestActions (Range.linear 1 100) (Vector.length (afterUndos ^. current)))
  -- and apply those
  afterNewActions <- runAndRecordAll afterUndos moreActions
  -- make sure we have no redos, and the new amount of undos
  numRedos afterNewActions === 0
  numUndos afterNewActions === length moreActions
