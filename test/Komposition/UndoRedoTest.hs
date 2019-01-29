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
  deriving (Show)

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
  :: (Monad m, Invertible action, Runnable action subject m)
  => (History action, subject)
  -> [Directed Forward action]
  -> m (History action, subject)
runAndRecordAll =
  foldM (\(history', state'') action -> runAndRecord action state'' history')

applyN
  :: (Monad m, MonadTest m, Invertible action, Runnable action subject m)
  => (History action -> subject -> Maybe (History action, m subject))
  -> (History action, subject)
  -> Int
  -> m (History action, subject)
applyN f acc n = foldM
  (\(history, state') _ -> case f history state' of
    Just (history', ma) -> (history', ) <$> ma
    Nothing             -> failure
  )
  acc
  (replicate n ())

hprop_undo_history_has_correct_number_of_undos = property $ do
  initial <- forAll (Gen.int Range.linearBounded)
  actions <- forAllWith (show . map unDirected) (genTestActions initial)
  (history, _) <- runAndRecordAll (init, initial) actions
  numUndos history === length actions

hprop_undo_all_returns_initial_state = property $ do
  initial <- forAll (Gen.int Range.linearBounded)
  actions <- forAllWith (show . map unDirected) (genTestActions initial)
  afterActions <- runAndRecordAll (init, initial) actions
  annotateShow (snd afterActions)
  numUndos (fst afterActions) === length actions
  (_, lastState) <- applyN undo afterActions (numUndos (fst afterActions))
  annotateShow lastState
  lastState === initial
