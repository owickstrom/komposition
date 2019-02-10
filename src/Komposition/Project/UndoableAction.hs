{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TupleSections         #-}
module Komposition.Project.UndoableAction where

import           Komposition.Prelude

import           Control.Lens

import           Komposition.Composition
import           Komposition.Composition.Delete as Delete
import           Komposition.Composition.Insert as Insert
import           Komposition.Duration
import           Komposition.Focus
import           Komposition.UndoRedo
import           Komposition.VideoSpeed

data UndoableAction
  = InsertAction (Focus 'SequenceFocusType) InsertPosition (Insertion ())
  | DeleteAction (Focus 'SequenceFocusType) DeletionOf
  | SetClipSpeed (Focus 'SequenceFocusType) VideoSpeed
  | SetClipStart (Focus 'SequenceFocusType) Duration
  | SetClipEnd (Focus 'SequenceFocusType) Duration
  deriving (Eq, Show, Generic)

instance MonadError Text m => Runnable UndoableAction (Timeline ()) (Focus 'SequenceFocusType) m where
  run action timeline = case action of
    DeleteAction oldFocus deletionOf ->
      case delete oldFocus deletionOf timeline of
        Nothing -> -- trace ("Can't delete" :: Text) $
          throwError "Can't delete at current focus."
        Just res@DeletionResult { inverseInsertion = (insertion, insertPos) } ->
          Delete.resultingTimeline res
            & (InsertAction (Delete.resultingFocus res) insertPos insertion, )
            & (, Delete.resultingFocus res)
            & pure
    InsertAction oldFocus pos insertion ->
      case insert_ oldFocus insertion pos timeline of
          Nothing -> -- trace ("Can't insert" :: Text) $
            throwError "Can't insert at current focus."
          Just (newTimeline, newFocus) ->
            newTimeline
              & (DeleteAction newFocus (inverseDeletionOf insertion), )
              & (, newFocus)
              & pure
    SetClipSpeed oldFocus speed ->
      case timeline ^? focusing oldFocus of
        Just (VideoClip _ _ _ oldSpeed) ->
          timeline
            &  focusing oldFocus %~ setClipSpeed
            &  (SetClipSpeed oldFocus oldSpeed, )
            &  (                              , oldFocus)
            &  pure
        _ -> throwError "Can't set clip speed at current focus"
      where
        setClipSpeed = \case
          VideoClip ann asset ts _ -> VideoClip ann asset ts speed
          gap                      -> gap
    SetClipStart oldFocus start ->
      case timeline ^? focusing oldFocus of
        Just (VideoClip _ _ ts _) ->
          timeline
            &  focusing oldFocus %~ setClipSpeed
            &  (SetClipStart oldFocus (spanStart ts), )
            &  (, oldFocus)
            &  pure
        _ -> throwError "Can't set clip start at current focus"
      where
        setClipSpeed = \case
          VideoClip ann asset ts speed ->
            VideoClip ann asset ts { spanStart = start } speed
          gap -> gap
    SetClipEnd oldFocus end ->
      case timeline ^? focusing oldFocus of
        Just (VideoClip _ _ ts _) ->
          timeline
            &  focusing oldFocus %~ setClipSpeed
            &  (SetClipEnd oldFocus (spanEnd ts), )
            &  (, oldFocus)
            &  pure
        _ -> throwError "Can't set clip end at current focus"
      where
        setClipSpeed = \case
          VideoClip ann asset ts speed ->
            VideoClip ann asset ts { spanEnd = end } speed
          gap -> gap

inverseDeletionOf :: Insertion a -> DeletionOf
inverseDeletionOf = \case
  InsertSequence _ -> DeletionOf 1
  InsertParallel _ -> DeletionOf 1
  InsertVideoParts vs -> DeletionOf (length vs)
  InsertAudioParts as -> DeletionOf (length as)
