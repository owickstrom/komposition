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

data UndoableAction
  = InsertAction (Focus 'SequenceFocusType) (Insertion ())
  | DeleteAction (Focus 'SequenceFocusType)
  deriving (Eq, Show)

instance MonadError Text m => Runnable UndoableAction UndoableState m where
  run action state' =
    case action of
      DeleteAction focus'           ->
        case delete_ focus' currentTimeline of
          Left (Just (_, _focusErr)) -> throwError "Delete failed."
          Left Nothing -> throwError "Can't delete at current focus."
          Right (timeline', deletedComposition', newFocus) ->
            state'
            & existingProject . project . timeline .~ timeline'
            & timelineFocus .~ newFocus
            & (InsertAction focus' (insertionFromSomeComposition deletedComposition'),)
            & pure
        where
          currentTimeline = state' ^. existingProject . project . timeline
      InsertAction focus' reinsertion ->
        state'
          & existingProject . project . timeline %~
            insert_
               focus'
               reinsertion
               LeftOf
          -- "insert left of" moves the focus right, so we reset to the previous focus manually
          & timelineFocus .~ focus'
          & (DeleteAction focus',)
          & pure
