{-# LANGUAGE ConstraintKinds     #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedLabels    #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE PolyKinds           #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE RebindableSyntax    #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}
module Komposition.Application.TimelineMode where

import           Komposition.Application.Base
import qualified Prelude

import           Control.Lens
import           Data.Row.Records                    hiding (split)
import           Data.String                         (fromString)
import           System.Directory
import           Text.Printf

import           Komposition.Composition
import           Komposition.Composition.Delete
import           Komposition.Composition.Insert
import           Komposition.Composition.Split
import           Komposition.Duration
import qualified Komposition.FFmpeg.Command          as FFmpeg
import           Komposition.Focus
import           Komposition.History
import           Komposition.MediaType
import           Komposition.Project
import           Komposition.Project.Store
import qualified Komposition.Render.Composition      as Render
import qualified Komposition.Render.FFmpeg           as Render

import           Komposition.Application.ImportMode
import           Komposition.Application.KeyMaps
import           Komposition.Application.LibraryMode

timelineMode
  :: Application t m
  => Name n
  -> TimelineModel
  -> t m (n .== State (t m) 'TimelineMode) Empty ()
timelineMode gui model = do
  updateTimeline gui model
  nextEventOrTimeout gui 5 >>= maybe resetStatusMessage onNextEvent
  where
    continue = timelineMode gui model
    continueWithStatusMessage msg =
      model
      & statusMessage .~ Just msg
      & timelineMode gui
    resetStatusMessage =
      model
      & statusMessage .~ Nothing
      & timelineMode gui
    onNextEvent = \case
      CommandKeyMappedEvent (FocusCommand cmd) ->
        case modifyFocus (currentProject model ^. timeline) cmd (model ^. currentFocus) of
          Left err -> do
            beep gui
            printUnexpectedFocusError err cmd
            continue
          Right newFocus -> timelineMode gui (model & currentFocus .~ newFocus)
      CommandKeyMappedEvent (JumpFocus newFocus) ->
        case atFocus newFocus (currentProject model ^. timeline) of
          Just _  -> timelineMode gui (model & currentFocus .~ newFocus)
          Nothing -> beep gui >>> continueWithStatusMessage "Couldn't set focus."
      CommandKeyMappedEvent (InsertCommand type' position) ->
        insertIntoTimeline gui model type' position
      CommandKeyMappedEvent Delete ->
        case delete (model ^. currentFocus) (currentProject model ^. timeline) of
          Nothing -> beep gui >> continueWithStatusMessage "Delete failed."
          Just (timeline', Just cmd) ->
            case modifyFocus
                  (currentProject model ^. timeline)
                  cmd
                  (model ^. currentFocus) of
              Left err -> do
                beep gui
                iliftIO (putStrLn ("Deleting failed: " <> show err :: Text))
                continueWithStatusMessage "Delete failed."
              Right newFocus ->
                model
                  & existingProject . projectHistory %~ edit (timeline .~ timeline')
                  & currentFocus .~ newFocus
                  & timelineMode gui
          Just (timeline', Nothing) ->
            model
              & existingProject . projectHistory %~ edit (timeline .~ timeline')
              & timelineMode gui
      CommandKeyMappedEvent Split ->
        case split (model ^. currentFocus) (currentProject model ^. timeline) of
          Just (timeline', newFocus) ->
            model
              & existingProject . projectHistory %~ edit (timeline .~ timeline')
              & currentFocus .~ newFocus
              & timelineMode gui
          Nothing -> do
            beep gui
            continueWithStatusMessage "Can't split composition at current focus."
      CommandKeyMappedEvent Import ->
        importFile gui model >>>= timelineMode gui
      CommandKeyMappedEvent Render ->
        case Render.flattenTimeline (currentProject model ^. timeline) of
          Just flat -> do
            outDir <- iliftIO getUserDocumentsDirectory
            chooseFile gui (Save File) "Render To File" outDir >>>= \case
              Just outFile -> do
                progressBar
                  gui
                  "Rendering"
                  (Render.renderComposition
                    (currentProject model ^. videoSettings)
                    Render.VideoOriginal
                    (FFmpeg.FileOutput outFile)
                    flat) >>= \case
                  Just (Right ()) -> continue
                  Just (Left (err :: Render.RenderError)) ->
                    iliftIO (print err) >>> continue
                  Nothing -> continue
              Nothing -> continue
          Nothing -> do
            beep gui
            continueWithStatusMessage "Cannot render a composition without video clips."
      CommandKeyMappedEvent Preview ->
        previewFocusedComposition gui model >>> continue
      CommandKeyMappedEvent Undo ->
        case model & existingProject . projectHistory %%~ undo of
          Just m  -> timelineMode gui m
          Nothing -> beep gui >> timelineMode gui model
      CommandKeyMappedEvent Redo ->
        case model & existingProject . projectHistory %%~ redo of
          Just m  -> timelineMode gui m
          Nothing -> beep gui >> timelineMode gui model
      CommandKeyMappedEvent SaveProject -> do
        iliftIO (saveExistingProject (model ^. existingProject)) >>= \case
          _ -> continue
      CommandKeyMappedEvent Cancel -> continue
      CommandKeyMappedEvent Help ->
        help gui [ModeKeyMap STimelineMode (keymaps STimelineMode)] >>> continue
      CommandKeyMappedEvent Exit ->
        dialog gui "Confirm Exit" "Are you sure you want to exit?" [No, Yes] >>>= \case
          Just Yes -> exit gui
          Just No -> continue
          Nothing -> continue
      ZoomLevelChanged zl -> model & zoomLevel .~ zl & timelineMode gui
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
  case (type', atFocus (model ^. currentFocus) (currentProject model ^. timeline)) of
    (InsertComposition, Just (FocusedSequence _)) ->
      model
      & existingProject . projectHistory %~ edit (timeline %~ insert_
                                              (model ^. currentFocus)
                                              (InsertParallel (Parallel () [] [])) RightOf)
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
      let msg = "Cannot perform " <> show c <> " when focused at " <> prettyFocusedAt f
      timelineMode gui (model & statusMessage .~ Just msg)
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
        &  existingProject . projectHistory %~ edit (timeline %~ insert_ (model ^. currentFocus) (gapInsertion seconds) position)
        &  ireturn
    Nothing -> ireturn model

prettyFocusedAt :: FocusedAt a -> Text
prettyFocusedAt = \case
  FocusedSequence{}  -> "sequence"
  FocusedParallel{}  -> "parallel"
  FocusedVideoPart{} -> "video track"
  FocusedAudioPart{} -> "audio track"

previewFocusedComposition
  :: Application t m
  => Name n
  -> TimelineModel
  -> Actions
       (t m)
       '[n := Remain (State (t m) TimelineMode)]
       r
       TimelineModel
previewFocusedComposition gui model =
  case flatComposition of
    Just flat -> do
       let streamingProcess =
             void $
             Render.renderComposition
               (currentProject model ^. proxyVideoSettings)
               Render.VideoProxy
               (FFmpeg.HttpStreamingOutput "localhost" 12345)
               flat
       _ <- previewStream gui "http://localhost:12345" streamingProcess (currentProject model ^. proxyVideoSettings)
       ireturn model
    Nothing -> do
      beep gui
      model
        & statusMessage .~ Just "Cannot preview a composition without video clips."
        & ireturn
  where
    flatComposition :: Maybe Render.Composition
    flatComposition = do
      atFocus (model ^. currentFocus) (currentProject model ^. timeline) Prelude.>>= \case
        FocusedSequence s -> Render.flattenSequence s
        FocusedParallel p -> Render.flattenParallel p
        _  -> Nothing
