{-# LANGUAGE ConstraintKinds     #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
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
import qualified Data.List.NonEmpty          as NonEmpty

import           Komposition.Composition
import           Komposition.Composition.Delete
import           Komposition.Composition.Insert
import           Komposition.Composition.Split
import           Komposition.Duration
import qualified Komposition.FFmpeg.Command          as FFmpeg
import           Komposition.Focus
import           Komposition.History
import           Komposition.Library
import           Komposition.MediaType
import           Komposition.Project
import           Komposition.Project.Store
import qualified Komposition.Render.Composition      as Render
import qualified Komposition.Render.FFmpeg           as Render

import           Komposition.Application.ImportMode
import           Komposition.Application.KeyMaps
import           Komposition.Application.LibraryMode

data TimelineModeResult
  = TimelineExit
  | TimelineClose

timelineMode
  :: ( Application t m
     , tm ~ (n .== State (t m) TimelineMode)
     )
  => Name n
  -> TimelineModel
  -> t m tm tm TimelineModeResult
timelineMode gui model = do
  updateTimeline gui model
  nextEventOrTimeout gui 5 >>= maybe resetStatusMessage onNextEvent
  where
    continue = timelineMode gui model
    continueWithStatusMessage msg =
      model
      & statusMessage ?~ msg
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
        selectFileToImport gui (addImportedAssetsToLibrary gui model)
      CommandKeyMappedEvent Render ->
        case Render.flattenTimeline (currentProject model ^. timeline) of
          Just flat -> do
            outDir <- iliftIO getUserDocumentsDirectory
            chooseFile gui (Save File) "Render To File" outDir >>>= \case
              Just outFile ->
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
      CommandKeyMappedEvent SaveProject ->
        iliftIO (saveExistingProject (model ^. existingProject)) >>= \case
          _ -> continue
      CommandKeyMappedEvent CloseProject -> ireturn TimelineClose
      CommandKeyMappedEvent Cancel -> continue
      CommandKeyMappedEvent Help ->
        help gui [ModeKeyMap STimelineMode (keymaps STimelineMode)] >>> continue
      CommandKeyMappedEvent Exit -> ireturn TimelineExit
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
  :: ( Application t m
     , tm ~ (n .== State (t m) TimelineMode)
     )
  => Name n
  -> TimelineModel
  -> InsertType
  -> InsertPosition
  -> t m tm tm TimelineModeResult
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
        Video -> selectAssetAndInsert gui model SVideo position
        Audio -> selectAssetAndInsert gui model SAudio position
    (InsertClip Nothing, Just FocusedVideoPart {}) ->
      selectAssetAndInsert gui model SVideo position
    (InsertClip Nothing, Just FocusedAudioPart {}) ->
      selectAssetAndInsert gui model SAudio position
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
      timelineMode gui (model & statusMessage ?~ msg)
    (_, Nothing) -> do
      iliftIO (putStrLn ("Warning: focus is invalid." :: Text))
      continue
  where
    continue = timelineMode gui model

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
        & statusMessage ?~ "Cannot preview a composition without video clips."
        & ireturn
  where
    flatComposition :: Maybe Render.Composition
    flatComposition =
      atFocus (model ^. currentFocus) (currentProject model ^. timeline) Prelude.>>= \case
        FocusedSequence s -> Render.flattenSequence s
        FocusedParallel p -> Render.flattenParallel p
        _  -> Nothing

noAssetsMessage :: SMediaType mt -> Text
noAssetsMessage mt =
  "You have no " <> mt' <> " assets in your library. Use 'Import' to add some assets."
  where
    mt' = case mt of
      SVideo -> "video"
      SAudio -> "audio"

selectAssetAndInsert ::
     (Application t m, r ~ (n .== State (t m) 'TimelineMode), IxPointed (t m))
  => Name n
  -> TimelineModel
  -> SMediaType mt
  -> InsertPosition
  -> t m r r TimelineModeResult
selectAssetAndInsert gui model mediaType' position =
  case mediaType' of
    SVideo ->
      case NonEmpty.nonEmpty (currentProject model ^. library . videoAssets) of
        Just vs -> selectAsset gui (SelectAssetsModel SVideo vs []) (insertSelectedAssets gui model SVideo position)
        Nothing -> onNoAssets gui SVideo
    SAudio ->
      case NonEmpty.nonEmpty (currentProject model ^. library . audioAssets) of
        Just as -> selectAsset gui (SelectAssetsModel SAudio as []) (insertSelectedAssets gui model SAudio position)
        Nothing -> onNoAssets gui SAudio
  where
    onNoAssets ::
        (Application t m, r ~ (n .== State (t m) 'TimelineMode), IxPointed (t m))
      => Name n
      -> SMediaType mt
      -> t m r r TimelineModeResult
    onNoAssets gui mediaType' = do
      beep gui
      model
        & statusMessage ?~ noAssetsMessage mediaType'
        & timelineMode gui

insertSelectedAssets ::
  ( ReturnsToTimeline mode
  , Application t m
  , HasType n (State (t m) mode) i
  , Modify n (State (t m) TimelineMode) i ~ o
  , o ~ ( n .==  State (t m) 'TimelineMode)
  )
  => Name n
  -> TimelineModel
  -> SMediaType mt
  -> InsertPosition
  -> Maybe [Asset mt]
  -> t m i o TimelineModeResult
insertSelectedAssets gui model mediaType' position result = do
  model' <-
    case result of
        Just assets -> do
          i <- insertionOf model mediaType' assets
          model
            & existingProject . projectHistory %~ edit (\p -> p & timeline %~ insert_ (model ^. currentFocus) i position)
            & ireturn
        Nothing -> do
          beep gui
          model
            & statusMessage ?~ noAssetsMessage mediaType'
            & ireturn
  returnToTimeline gui model'
  timelineMode gui model'

insertionOf ::
     (IxMonad (t m), IxMonadIO (t m))
  => TimelineModel
  -> SMediaType mt
  -> [Asset mt]
  -> t m r r (Insertion ())
insertionOf model SVideo a = iliftIO (InsertVideoParts <$> mapM (toVideoClip model) a)
insertionOf _ SAudio a = ireturn (InsertAudioParts (AudioClip () <$> a))

toVideoClip :: TimelineModel -> VideoAsset -> IO (VideoPart ())
toVideoClip model videoAsset =
  let ts =
        maybe
          (TimeSpan 0 (durationOf videoAsset))
          snd
          (videoAsset ^. videoClassifiedScene)
  in VideoClip () videoAsset ts <$>
      Render.extractFrameToFile
        (currentProject model ^. videoSettings)
        Render.FirstFrame
        Render.VideoProxy
        videoAsset
        ts
        (model ^. existingProject . projectPath . unProjectPath)

addImportedAssetsToLibrary ::
  ( ReturnsToTimeline mode
  , Application t m
  , HasType n (State (t m) mode) i
  , Modify n (State (t m) TimelineMode) i ~ o
  , o ~ ( n .==  State (t m) 'TimelineMode)
  )
  => Name n
  -> TimelineModel
  -> Maybe (FilePath, Bool)
  -> t m i o TimelineModeResult
addImportedAssetsToLibrary gui model (Just selected) = do
  returnToTimeline gui model
  model' <- importSelectedFile gui (model ^. existingProject) selected >>>= \case
    Just (Left err) -> do
      iliftIO (print err)
      _ <- dialog gui "Import Failed!" (show err) [Ok]
      ireturn model
    Just (Right (Left vs)) ->
      model
        & existingProject . projectHistory %~ edit (library . videoAssets %~ (<> vs))
        & ireturn
    Just (Right (Right as)) ->
      model
        & existingProject . projectHistory %~ edit (library . audioAssets %~ (<> as))
        & ireturn
    Nothing -> ireturn model
  timelineMode gui model'
addImportedAssetsToLibrary gui model Nothing = do
  returnToTimeline gui model
  timelineMode gui model
