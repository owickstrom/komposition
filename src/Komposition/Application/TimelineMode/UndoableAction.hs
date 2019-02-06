{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE StandaloneDeriving    #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TupleSections         #-}
module Komposition.Application.TimelineMode.UndoableAction where

import           Komposition.Prelude

import           Control.Lens

import           Komposition.Composition.Delete
import           Komposition.Composition.Insert
import           Komposition.Focus
import           Komposition.Project
import           Komposition.UndoRedo


data UndoableState = UndoableState
  { _existingProject :: ExistingProject
  , _timelineFocus   :: Focus 'SequenceFocusType
  }
  deriving (Eq, Show)

makeLenses ''UndoableState

data UndoableAction dir where
  DeleteAction :: UndoableAction 'Forward
  UnDeleteAction :: Focus 'SequenceFocusType -> Insertion () -> UndoableAction 'Backward
deriving instance Eq (UndoableAction dir)
deriving instance Show (UndoableAction dir)

instance MonadError Text m => Runnable UndoableAction UndoableState m where
  run action state' =
    case action of
      DeleteAction ->
            case delete_ currentFocus' currentTimeline of
              Left (Just (_, _focusErr)) -> throwError "Delete failed."
              Left Nothing -> throwError "Can't delete at current focus."
              Right (timeline', deletedComposition', focus') ->
                state'
                & existingProject . project . timeline .~ timeline'
                & timelineFocus .~ focus'
                & (UnDeleteAction currentFocus' (insertionFromSomeComposition deletedComposition'),)
                & pure
        where
          currentFocus' = state' ^. timelineFocus
          currentTimeline = state' ^. existingProject . project . timeline
  revert action state' =
    case action of
      UnDeleteAction focus' reinsertion ->
        state'
          & existingProject . project . timeline %~
            insert_
               focus'
               reinsertion
               LeftOf
          -- "insert left of" moves the focus right, so we reset to the previous focus manually
          & timelineFocus .~ focus'
          & (DeleteAction,)
          & pure

