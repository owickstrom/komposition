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
import qualified FastCut.FFmpeg.Command          as FFmpeg
import qualified FastCut.Render.Composition      as Render
import qualified FastCut.Render.FFmpeg           as Render

import           FastCut.Application.ImportMode
import           FastCut.Application.KeyMaps
import           FastCut.Application.LibraryMode

timelineMode
  :: Application t m
  => Name n
  -> TimelineModel
  -> t m (n .== State (t m) 'TimelineMode) Empty ()
timelineMode gui model = do
  updateTimeline gui model
  nextEvent gui >>>= \case
    CommandKeyMappedEvent (FocusCommand cmd) ->
      case modifyFocus (model ^. project . timeline) cmd (model ^. currentFocus) of
        Left err -> do
          beep gui
          printUnexpectedFocusError err cmd
          continue
        Right newFocus -> timelineMode gui (model & currentFocus .~ newFocus)
    CommandKeyMappedEvent (JumpFocus newFocus) ->
      case atFocus newFocus (model ^. project . timeline) of
        Just _  -> timelineMode gui (model & currentFocus .~ newFocus)
        Nothing -> beep gui >>> continue
    CommandKeyMappedEvent (InsertCommand type' position) ->
      insertIntoTimeline gui model type' position
    CommandKeyMappedEvent Delete ->
      case delete (model ^. currentFocus) (model ^. project . timeline) of
        Nothing -> beep gui >> continue
        Just (timeline', Just cmd) ->
          case modifyFocus
                 (model ^. project . timeline)
                 cmd
                 (model ^. currentFocus) of
            Left err -> do
              beep gui
              iliftIO (putStrLn ("Deleting failed: " <> show err :: Text))
              continue
            Right newFocus ->
              model & project . timeline .~ timeline' & currentFocus .~ newFocus &
              timelineMode gui
        Just (timeline', Nothing) ->
          model & project . timeline .~ timeline' & timelineMode gui
    CommandKeyMappedEvent Split ->
      case split (model ^. currentFocus) (model ^. project . timeline) of
        Just (timeline', newFocus) ->
          model & project . timeline .~ timeline' & currentFocus .~ newFocus &
          timelineMode gui
        Nothing -> beep gui >> continue
    CommandKeyMappedEvent Import -> importFile gui model >>>= timelineMode gui
    CommandKeyMappedEvent Render ->
      case Render.flattenTimeline (model ^. project . timeline) of
        Just flat -> do
          outDir <- iliftIO getUserDocumentsDirectory
          chooseFile gui Save "Render To File" outDir >>>= \case
            Just outFile -> do
              progressBar
                gui
                "Rendering"
                (Render.renderComposition
                   (model ^. project . videoSettings)
                   Render.VideoOriginal
                   (FFmpeg.FileOutput outFile)
                   flat) >>= \case
                Just Render.Success -> continue
                Just (Render.ProcessFailed err) ->
                  iliftIO (putStrLn err) >>> continue
                Nothing -> continue
            Nothing -> continue
        Nothing -> beep gui >>> continue
    CommandKeyMappedEvent Preview ->
      case Render.flattenTimeline (model ^. project . timeline) of
        Just flat -> do
          let streamingProcess =
                void $
                Render.renderComposition
                  (model ^. project . proxyVideoSettings)
                  Render.VideoProxy
                  (FFmpeg.HttpStreamingOutput "localhost" 12345)
                  flat
          _ <- previewStream gui "http://localhost:12345" streamingProcess (model ^. project . proxyVideoSettings)
          continue
        Nothing -> beep gui >>> continue
    CommandKeyMappedEvent Cancel -> continue
    CommandKeyMappedEvent Help ->
      help gui [ModeKeyMap STimelineMode (keymaps STimelineMode)] >>> continue
    CommandKeyMappedEvent Exit ->
      dialog gui "Confirm Exit" "Are you sure you want to exit?" [No, Yes] >>>= \case
        Just Yes -> exit gui
        Just No -> continue
        Nothing -> continue
    ZoomLevelChanged zl -> model & zoomLevel .~ zl & timelineMode gui
  where
    continue = timelineMode gui model
    printUnexpectedFocusError err cmd =
      case err of
        UnhandledFocusModification {} ->
          iliftIO
            (printf
               "Error: could not handle focus modification %s\n"
               (show cmd :: Text))
        _ -> ireturn ()

insertIntoTimeline
  :: Application t m
  => Name n
  -> TimelineModel
  -> InsertType
  -> InsertPosition
  -> t m (n .== State (t m) 'TimelineMode) Empty ()
insertIntoTimeline gui model type' position =
  case (type', atFocus (model ^. currentFocus) (model ^. project . timeline)) of
    (InsertComposition, Just (FocusedSequence _)) ->
      model
      & project . timeline %~ insert_ (model ^. currentFocus) (InsertParallel (Parallel () [] [])) RightOf
      & timelineMode gui
    (InsertClip (Just mt), Just FocusedParallel {}) ->
      case mt of
        Video -> selectAssetAndInsert gui model SVideo position >>>= timelineMode gui
        Audio -> selectAssetAndInsert gui model SAudio position >>>= timelineMode gui
    (InsertClip Nothing, Just FocusedVideoPart {}) ->
      selectAssetAndInsert gui model SVideo position >>>= timelineMode gui
    (InsertClip Nothing, Just FocusedAudioPart {}) ->
      selectAssetAndInsert gui model SAudio position >>>= timelineMode gui
    (InsertGap (Just mt), Just FocusedParallel {}) ->
      case mt of
        Video -> insertGap gui model SVideo position >>>= timelineMode gui
        Audio -> insertGap gui model SAudio position >>>= timelineMode gui
    (InsertGap Nothing, Just FocusedVideoPart {}) ->
      insertGap gui model SVideo position >>>= timelineMode gui
    (InsertGap Nothing, Just FocusedAudioPart {}) ->
      insertGap gui model SAudio position >>>= timelineMode gui
    (c, Just f) -> do
      iliftIO
        (putStrLn
           ("Cannot perform " <> show c <> " when focused at " <>
            prettyFocusedAt f))
      continue
    (_, Nothing) -> do
      iliftIO (putStrLn ("Warning: focus is invalid." :: Text))
      continue
  where
    continue = timelineMode gui model

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
  -> TimelineModel
  -> SMediaType mt
  -> InsertPosition
  -> Actions
       (t m)
       '[n := Remain (State (t m) TimelineMode)]
       r
       TimelineModel
insertGap gui model mediaType' position = do
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
      model
        &  project . timeline
        %~ insert_ (model ^. currentFocus) (gapInsertion seconds) position
        &  ireturn
    Nothing -> ireturn model

prettyFocusedAt :: FocusedAt a -> Text
prettyFocusedAt = \case
  FocusedSequence{}  -> "sequence"
  FocusedParallel{}  -> "parallel"
  FocusedVideoPart{} -> "video track"
  FocusedAudioPart{} -> "audio track"
