{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE StandaloneDeriving    #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TupleSections         #-}
module Komposition.Application.TimelineMode.UndoableAction where

import           Komposition.Prelude

import           Control.Lens

import           Komposition.Composition.Delete as Delete
import           Komposition.Composition.Insert as Insert
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
  = InsertAction (Focus 'SequenceFocusType) InsertPosition (Insertion ())
  | DeleteAction (Focus 'SequenceFocusType) DeletionOf
  deriving (Eq, Show)

instance MonadError Text m => Runnable UndoableAction UndoableState m where
  run action state' =
    case action of
      DeleteAction oldFocus           deletionOf ->
        case delete oldFocus deletionOf currentTimeline of
          Nothing -> -- trace ("Can't delete" :: Text) $
            throwError "Can't delete at current focus."
          Just res@DeletionResult{inverseInsertion = (insertion, insertPos)} ->
            state'
                & existingProject . project . timeline .~ Delete.resultingTimeline res
                & timelineFocus .~ Delete.resultingFocus res
                & (InsertAction (Delete.resultingFocus res) insertPos insertion, )
                & pure
        where
          currentTimeline = state' ^. existingProject . project . timeline
      InsertAction oldFocus pos insertion ->
        case insert_ oldFocus insertion pos (state'^.existingProject.project.timeline) of
          Nothing -> -- trace ("Can't insert" :: Text) $
            throwError "Can't insert at current focus."
          Just (newTimeline, newFocus) ->
            state'
              & existingProject . project . timeline .~ newTimeline
              & timelineFocus .~ newFocus
              & (DeleteAction newFocus (inverseDeletionOf insertion),)
              & pure

inverseDeletionOf :: Insertion a -> DeletionOf
inverseDeletionOf = \case
  InsertSequence _ -> DeletionOf 1
  InsertParallel _ -> DeletionOf 1
  InsertVideoParts vs -> DeletionOf (length vs)
  InsertAudioParts as -> DeletionOf (length as)
