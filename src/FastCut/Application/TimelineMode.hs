{-# OPTIONS_GHC -fno-warn-unticked-promoted-constructors #-}

{-# LANGUAGE ConstraintKinds   #-}
{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedLabels  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds         #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE RebindableSyntax  #-}
{-# LANGUAGE TypeOperators     #-}
module FastCut.Application.TimelineMode where

import           FastCut.Application.Base

import           Control.Lens
import           Data.Row.Records                hiding (split)
import           Data.String                     (fromString)
import           System.Directory
import           Text.Printf

import           FastCut.Composition
import           FastCut.Composition.Delete
import           FastCut.Composition.Insert
import           FastCut.Composition.Split
import           FastCut.Duration
import           FastCut.Focus
import           FastCut.MediaType
import           FastCut.Project
import qualified FastCut.Render.Composition      as Render
import qualified FastCut.Render.FFmpeg           as Render

import           FastCut.Application.ImportMode
import           FastCut.Application.KeyMaps
import           FastCut.Application.LibraryMode

timelineMode
  :: Application t m
  => Name n
  -> Focus SequenceFocusType
  -> Project
  -> t m (n .== State (t m) 'TimelineMode) Empty ()
timelineMode gui focus' project = do
  updateTimeline gui project focus'
  nextEvent gui >>>= \case
    CommandKeyMappedEvent (FocusCommand cmd) ->
      case modifyFocus (project ^. timeline) cmd focus' of
        Left err -> do
          beep gui
          printUnexpectedFocusError err cmd
          continue
        Right newFocus -> timelineMode gui newFocus project
    CommandKeyMappedEvent (JumpFocus newFocus) ->
      case atFocus newFocus (project ^. timeline) of
        Just _  -> timelineMode gui newFocus project
        Nothing -> beep gui >>> continue
    CommandKeyMappedEvent (InsertCommand type' position) ->
      insertIntoTimeline gui project focus' type' position
    CommandKeyMappedEvent Delete -> case delete focus' (project ^. timeline) of
      Nothing -> beep gui >> continue
      Just (timeline', Just cmd) ->
        case modifyFocus (project ^. timeline) cmd focus' of
          Left err -> do
            beep gui
            iliftIO (putStrLn ("Deleting failed: " <> show err :: Text))
            continue
          Right newFocus ->
            project & timeline .~ timeline' & timelineMode gui newFocus
      Just (timeline', Nothing) ->
        project & timeline .~ timeline' & timelineMode gui focus'
    CommandKeyMappedEvent Split -> case split focus' (project ^. timeline) of
      Just (timeline', newFocus) ->
        project & timeline .~ timeline' & timelineMode gui newFocus
      Nothing -> beep gui >> continue
    CommandKeyMappedEvent Import ->
      importFile gui project focus' >>>= timelineMode gui focus'
    CommandKeyMappedEvent Render ->
      case Render.flattenTimeline (project ^. timeline) of
        Just flat -> do
          outDir <- iliftIO getUserDocumentsDirectory
          chooseFile gui Save "Render To File" outDir >>>= \case
            Just outFile -> do
              progressBar gui
                          "Rendering"
                          (Render.renderComposition 25 outFile flat)
                >>= \case
                      Just Render.Success -> continue
                      Just (Render.ProcessFailed err) ->
                        iliftIO (putStrLn err) >>> continue
                      Nothing -> continue
            Nothing -> continue
        Nothing -> beep gui >>> continue
    CommandKeyMappedEvent Cancel -> continue
    CommandKeyMappedEvent Help ->
      help gui [ModeKeyMap STimelineMode (keymaps STimelineMode)] >>> continue
    CommandKeyMappedEvent Exit ->
      dialog gui "Confirm Exit" "Are you sure you want to exit?" [No, Yes]
        >>>= \case
               Just Yes -> exit gui
               Just No  -> continue
               Nothing  -> continue
  where
    continue = timelineMode gui focus' project
    printUnexpectedFocusError err cmd = case err of
      UnhandledFocusModification{} -> iliftIO
        (printf "Error: could not handle focus modification %s\n"
                (show cmd :: Text)
        )
      _ -> ireturn ()

insertIntoTimeline
  :: Application t m
  => Name n
  -> Project
  -> Focus SequenceFocusType
  -> InsertType
  -> InsertPosition
  -> t m (n .== State (t m) 'TimelineMode) Empty ()
insertIntoTimeline gui project focus' type' position =
  case (type', atFocus focus' (project ^. timeline)) of
    (InsertComposition, Just (FocusedSequence _)) ->
      selectAsset gui project focus' SVideo >>= \case
        Just assets ->
          project
            &  timeline
            %~ insert_ focus'
                       (InsertParallel (Parallel () (VideoClip () <$> assets) []))
                       RightOf
            &  timelineMode gui focus'
        Nothing -> beep gui >>> continue
    (InsertClip Video, Just f) | validClipInsertFocus f ->
      selectAssetAndInsert gui project focus' SVideo position
        >>>= timelineMode gui focus'
    (InsertClip Audio, Just f) | validClipInsertFocus f ->
      selectAssetAndInsert gui project focus' SAudio position
        >>>= timelineMode gui focus'
    (InsertGap Video, Just f) | validClipInsertFocus f  ->
      insertGap gui project focus' SVideo position >>>= timelineMode gui focus'
    (InsertGap Audio, Just f) | validClipInsertFocus f  ->
      insertGap gui project focus' SAudio position >>>= timelineMode gui focus'
    (c, Just f) -> do
      iliftIO
        (putStrLn
          (  "Cannot perform "
          <> show c
          <> " when focused at "
          <> prettyFocusedAt f
          )
        )
      continue
    (_, Nothing) -> do
      iliftIO (putStrLn ("Warning: focus is invalid." :: Text))
      continue
  where continue = timelineMode gui focus' project
        validClipInsertFocus = \case
          FocusedParallel{} -> True
          FocusedVideoPart{} -> True
          FocusedAudioPart{} -> True
          _ -> False

data Confirmation
  = Yes
  | No
  deriving (Show, Eq, Enum)

instance DialogChoice Confirmation where
  toButtonLabel = \case
    Yes -> "Yes"
    No -> "No"

insertGap
  :: Application t m
  => Name n
  -> Project
  -> Focus (ToFocusType Timeline)
  -> SMediaType mt
  -> InsertPosition
  -> Actions
       (t m)
       '[n := Remain (State (t m) TimelineMode)]
       r
       Project
insertGap gui project focus' mediaType' position = do
  gapDuration <- prompt gui
                        "Insert Gap"
                        "Please specify a gap duration in seconds."
                        "Insert Gap"
                        (NumberPrompt (0.1, 10e10))
  let gapInsertion seconds = case mediaType' of
        SVideo -> (InsertVideoParts [VideoGap () (durationFromSeconds seconds)])
        SAudio -> (InsertAudioParts [AudioGap () (durationFromSeconds seconds)])
  case gapDuration of
    Just seconds ->
      project
        &  timeline
        %~ insert_ focus' (gapInsertion seconds) position
        &  ireturn
    Nothing -> ireturn project

prettyFocusedAt :: FocusedAt a -> Text
prettyFocusedAt = \case
  FocusedSequence{}  -> "sequence"
  FocusedParallel{}  -> "parallel"
  FocusedVideoPart{} -> "video track"
  FocusedAudioPart{} -> "audio track"
