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

import           Control.Effect                      (Member)
import           Control.Effect.Carrier              (Carrier)
import           Control.Lens
import qualified Data.List.NonEmpty                  as NonEmpty
import           Data.Row.Records                    hiding (split)
import           Data.String                         (fromString)

import           Komposition.Application.Form
import           Komposition.Composition
import           Komposition.Composition.Delete
import           Komposition.Composition.Insert
import           Komposition.Composition.Paste
import           Komposition.Composition.Split
import           Komposition.Duration
import           Komposition.Focus
import           Komposition.History
import           Komposition.Import.Audio
import           Komposition.Import.Video
import           Komposition.Library
import           Komposition.MediaType
import           Komposition.Project
import           Komposition.Project.Store
import           Komposition.Render
import qualified Komposition.Render.Composition      as Render
import           Komposition.UserInterface.Dialog
import           Komposition.UserInterface.Help
import           Komposition.VideoSettings

import           Komposition.Application.ImportMode
import           Komposition.Application.KeyMaps
import           Komposition.Application.LibraryMode

data TimelineModeResult
  = TimelineExit TimelineModel
  | TimelineClose

type TimelineEffects sig =
  ( Member ProjectStore sig
  , Member VideoImport sig
  , Member AudioImport sig
  , Member Render sig
  )

timelineMode
  :: ( Application t m sig
     , TimelineEffects sig
     , Carrier sig m
     , r ~ (n .== Window (t m) (Event TimelineMode))
     )
  => Name n
  -> TimelineModel
  -> t m r r TimelineModeResult
timelineMode gui model = do
  patchWindow gui (timelineView model)
  nextEventOrTimeout gui 5 >>= maybe resetStatusMessage onNextEvent
  where
    continue = timelineMode gui model
    continueWithStatusMessage msg =
      model & statusMessage ?~ msg & timelineMode gui
    resetStatusMessage = model & statusMessage .~ Nothing & timelineMode gui
    onNextEvent        = \case
      CommandKeyMappedEvent (FocusCommand cmd) ->
        case
            modifyFocus (currentProject model ^. timeline)
                        cmd
                        (model ^. currentFocus)
          of
            Left err -> do
              beep gui
              printUnexpectedFocusError err cmd
              continue
            Right newFocus ->
              model
                & currentFocus .~ newFocus
                & timelineMode gui
      CommandKeyMappedEvent (JumpFocus newFocus) ->
        case atFocus newFocus (currentProject model ^. timeline) of
          Just _ -> timelineMode gui (model & currentFocus .~ newFocus)
          Nothing ->
            beep gui >>> continueWithStatusMessage "Couldn't set focus."
      CommandKeyMappedEvent (InsertCommand type' position) ->
        insertIntoTimeline gui model type' position
      CommandKeyMappedEvent Delete -> deleteFocused gui model
      CommandKeyMappedEvent Copy ->
        model
          &  clipboard
          .~ atFocus (model ^. currentFocus) (currentProject model ^. timeline)
          &  timelineMode gui
      CommandKeyMappedEvent (Paste pos) -> case model ^. clipboard of
        Nothing -> beep gui >>> continue
        Just cb ->
          case
              paste (model ^. currentFocus)
                    cb
                    pos
                    (currentProject model ^. timeline)
            of
              Just timeline' ->
                model
                  &  existingProject
                  .  projectHistory
                  %~ edit (timeline .~ timeline')
                  &  timelineMode gui
              Nothing ->
                beep gui >> continueWithStatusMessage "Couldn't paste."
      CommandKeyMappedEvent Split ->
        case split (model ^. currentFocus) (currentProject model ^. timeline) of
          Just (timeline', newFocus) ->
            model
              &  existingProject
              .  projectHistory
              %~ edit (timeline .~ timeline')
              &  currentFocus
              .~ newFocus
              &  timelineMode gui
          Nothing -> do
            beep gui
            continueWithStatusMessage
              "Can't split composition at current focus."
      CommandKeyMappedEvent Import ->
        selectFileToImport >>>= addImportedAssetsToLibrary gui model
      CommandKeyMappedEvent Render ->
        case Render.flattenTimeline (currentProject model ^. timeline) of
          Just flat -> do
            outDir <- ilift getDefaultProjectsDirectory
            chooseFile gui (Save File) "Render To File" outDir >>>= \case
              Just outFile -> do
                stream <- ilift $ renderComposition
                  (currentProject model ^. videoSettings . renderVideoSettings)
                  VideoTranscoded
                  (FileOutput outFile)
                  flat
                progressBar gui "Rendering" stream >>= \case
                  Just (Right ()) -> continue
                  Just (Left (SomeException err)) ->
                    ilift (logLnShow Error err) >>> continue
                  Nothing -> continue
              Nothing -> continue
          Nothing -> do
            beep gui
            continueWithStatusMessage
              "Cannot render a composition without video clips."
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
        ilift (saveExistingProject (model ^. existingProject)) >>= \case
          _ -> continue
      CommandKeyMappedEvent CloseProject -> ireturn TimelineClose
      CommandKeyMappedEvent Cancel       -> continue
      CommandKeyMappedEvent Help ->
        help gui [ModeKeyMap STimelineMode (keymaps STimelineMode)] >>>= \case
          Just HelpClosed -> continue
          Nothing         -> continue
      CommandKeyMappedEvent Exit -> ireturn (TimelineExit model)
      ZoomLevelChanged      zl   -> model & zoomLevel .~ zl & timelineMode gui
      FocusedClipStartSet start ->
        model
        & existingProject . projectHistory
        %~ edit (timeline . focusing (model ^. currentFocus) . _VideoPart
                 %~ setVideoStart)
        &  timelineMode gui
        where
          setVideoStart = \case
            VideoClip ann asset ts speed path' -> VideoClip ann asset ts { spanStart = start } speed path'
            vg@VideoGap{} -> vg
      WindowClosed               -> ireturn (TimelineExit model)

    printUnexpectedFocusError err cmd = case err of
      UnhandledFocusModification{} ->
        ilift
          (logLnText Warning
                     ("Could not handle focus modification: " <> show cmd)
          )
      _ -> ireturn ()

insertIntoTimeline
  :: ( Application t m sig
     , TimelineEffects sig
     , Carrier sig m
     , r ~ (n .== Window (t m) (Event TimelineMode))
     )
  => Name n
  -> TimelineModel
  -> InsertType
  -> InsertPosition
  -> t m r r TimelineModeResult
insertIntoTimeline gui model type' position =
  case
      ( type'
      , atFocus (model ^. currentFocus) (currentProject model ^. timeline)
      )
    of
      (InsertComposition, Just (SomeSequence _)) ->
        model
          &  existingProject
          .  projectHistory
          %~ edit
               (  timeline
               %~ insert_ (model ^. currentFocus)
                          (InsertParallel (Parallel () [] []))
                          RightOf
               )
          &  timelineMode gui
      (InsertClip (Just mt), Just SomeParallel{}) -> case mt of
        Video -> selectAssetAndInsert gui model SVideo position
        Audio -> selectAssetAndInsert gui model SAudio position
      (InsertClip Nothing, Just SomeVideoPart{}) ->
        selectAssetAndInsert gui model SVideo position
      (InsertClip Nothing, Just SomeAudioPart{}) ->
        selectAssetAndInsert gui model SAudio position
      (InsertGap (Just mt), Just SomeParallel{}) -> case mt of
        Video -> insertGap gui model SVideo position >>>= timelineMode gui
        Audio -> insertGap gui model SAudio position >>>= timelineMode gui
      (InsertGap Nothing, Just SomeVideoPart{}) ->
        insertGap gui model SVideo position >>>= timelineMode gui
      (InsertGap Nothing, Just SomeAudioPart{}) ->
        insertGap gui model SAudio position >>>= timelineMode gui
      (c, Just f) -> do
        let
          msg =
            "Cannot perform "
              <> show c
              <> " when focused at "
              <> prettyFocusedAt f
        timelineMode gui (model & statusMessage ?~ msg)
      (_, Nothing) -> do
        ilift (logLnText Warning "Focus is invalid.")
        continue
  where continue = timelineMode gui model

insertGap
  :: (Application t m sig, HasType parent (Window (t m) parentEvent) r)
  => Name parent
  -> TimelineModel
  -> SMediaType mt
  -> InsertPosition
  -> t m r r TimelineModel
insertGap parent model mediaType' position = do
  gapDuration <- prompt parent
                        "Insert Gap"
                        "Please specify a gap duration in seconds."
                        "Insert Gap"
                        (PromptNumber (0.1, 10e10, 0.1))
  let gapInsertion seconds = case mediaType' of
        SVideo -> InsertVideoParts [VideoGap () (durationFromSeconds seconds)]
        SAudio -> InsertAudioParts [AudioGap () (durationFromSeconds seconds)]
  case gapDuration of
    Just seconds ->
      model
        &  existingProject
        .  projectHistory
        %~ edit
             (  timeline
             %~ insert_ (model ^. currentFocus)
                        (gapInsertion seconds)
                        position
             )
        &  ireturn
    Nothing -> ireturn model

prettyFocusedAt :: FocusedAt a -> Text
prettyFocusedAt = \case
  SomeSequence{}  -> "sequence"
  SomeParallel{}  -> "parallel"
  SomeVideoPart{} -> "video track"
  SomeAudioPart{} -> "audio track"

previewFocusedComposition
  :: ( Application t m sig
     , HasType n (Window (t m) e) r
     , Carrier sig m
     , TimelineEffects sig
     )
  => Name n
  -> TimelineModel
  -> t m r r TimelineModel
previewFocusedComposition gui model = case flatComposition of
  Just flat -> do
    streamingProcess <- ilift $ renderComposition
      (currentProject model ^. videoSettings . proxyVideoSettings)
      VideoProxy
      (HttpStreamingOutput "localhost" 12345)
      flat
    _ <- previewStream
      gui
      "http://localhost:12345"
      streamingProcess
      (currentProject model ^. videoSettings . proxyVideoSettings)
    ireturn model
  Nothing -> do
    beep gui
    model
      &  statusMessage
      ?~ "Cannot preview a composition without video clips."
      &  ireturn
  where
    flatComposition :: Maybe Render.Composition
    flatComposition =
      atFocus (model ^. currentFocus) (currentProject model ^. timeline)
        Prelude.>>= \case
                      SomeSequence s -> Render.flattenSequence s
                      SomeParallel p -> Render.flattenParallel p
                      _              -> Nothing

noAssetsMessage :: SMediaType mt -> Text
noAssetsMessage mt =
  "You have no "
    <> mt'
    <> " assets in your library. Use 'Import' to add some assets."
  where
    mt' = case mt of
      SVideo -> "video"
      SAudio -> "audio"

selectAssetAndInsert
  :: ( Application t m sig
     , TimelineEffects sig
     , Carrier sig m
     , r ~ (n .== Window (t m) (Event TimelineMode))
     )
  => Name n
  -> TimelineModel
  -> SMediaType mt
  -> InsertPosition
  -> t m r r TimelineModeResult
selectAssetAndInsert gui model mediaType' position = case mediaType' of
  SVideo ->
    case NonEmpty.nonEmpty (currentProject model ^. library . videoAssets) of
      Just vs ->
        selectAsset (SelectAssetsModel SVideo vs [])
          >>>= insertSelectedAssets gui model SVideo position
      Nothing -> onNoAssets gui SVideo
  SAudio ->
    case NonEmpty.nonEmpty (currentProject model ^. library . audioAssets) of
      Just as ->
        selectAsset (SelectAssetsModel SAudio as [])
          >>>= insertSelectedAssets gui model SAudio position
      Nothing -> onNoAssets gui SAudio
  where
    onNoAssets
      :: ( Application t m sig
         , TimelineEffects sig
         , Carrier sig m
         , r ~ (n .== Window (t m) (Event 'TimelineMode))
         , IxPointed (t m)
         )
      => Name n
      -> SMediaType mt
      -> t m r r TimelineModeResult
    onNoAssets gui' mt = do
      beep gui'
      model & statusMessage ?~ noAssetsMessage mt & timelineMode gui'

insertSelectedAssets
  :: ( Application t m sig
     , Carrier sig m
     , TimelineEffects sig
     , r ~ (n .== Window (t m) (Event TimelineMode))
     )
  => Name n
  -> TimelineModel
  -> SMediaType mt
  -> InsertPosition
  -> Maybe [Asset mt]
  -> t m r r TimelineModeResult
insertSelectedAssets gui model mediaType' position result = do
  model' <- case result of
    Just assets -> do
      i <- insertionOf model mediaType' assets
      model
        &  existingProject
        .  projectHistory
        %~ edit
             (\p -> p & timeline %~ insert_ (model ^. currentFocus) i position)
        &  ireturn
    Nothing -> do
      beep gui
      model & statusMessage ?~ noAssetsMessage mediaType' & ireturn
  timelineMode gui model'

insertionOf
  :: (IxMonadTrans t, IxMonad (t m), Monad m, Carrier sig m, Member Render sig)
  => TimelineModel
  -> SMediaType mt
  -> [Asset mt]
  -> t m r r (Insertion ())
insertionOf model SVideo a =
  ilift (InsertVideoParts <$> mapM (toVideoClip model) a)
insertionOf _ SAudio a = ireturn (InsertAudioParts (AudioClip () <$> a))

toVideoClip
  :: (Monad m, Carrier sig m, Member Render sig)
  => TimelineModel
  -> VideoAsset
  -> m (VideoPart ())
toVideoClip model videoAsset =
  let ts = maybe (TimeSpan 0 (durationOf OriginalDuration videoAsset))
                 snd
                 (videoAsset ^. videoClassifiedScene)
      speed = videoAsset ^. videoSpeed
  in  VideoClip () videoAsset ts speed <$> extractFrameToFile
        (currentProject model ^. videoSettings . proxyVideoSettings)
        Render.FirstFrame
        VideoProxy
        videoAsset
        ts
        (model ^. existingProject . projectPath . unProjectPath)

addImportedAssetsToLibrary
  :: ( Application t m sig
     , Carrier sig m
     , TimelineEffects sig
     , r ~ (n .== Window (t m) (Event TimelineMode))
     )
  => Name n
  -> TimelineModel
  -> Maybe (ImportFileForm Valid)
  -> t m r r TimelineModeResult
addImportedAssetsToLibrary gui model (Just selected) = do
  model' <-
    importSelectedFile gui (model ^. existingProject) selected >>>= \case
      Just (Left err) -> do
        ilift (logLnShow Error err)
        _ <- dialog
          gui
          DialogProperties
            { dialogTitle   = "Import Failed!"
            , dialogMessage = show err
            , dialogChoices = [Ok]
            }
        ireturn model
      Just (Right (Left vs)) ->
        model
          &  existingProject
          .  projectHistory
          %~ edit (library . videoAssets %~ (<> vs))
          &  ireturn
      Just (Right (Right as)) ->
        model
          &  existingProject
          .  projectHistory
          %~ edit (library . audioAssets %~ (<> as))
          &  ireturn
      Nothing -> ireturn model
  timelineMode gui model'
addImportedAssetsToLibrary gui model Nothing = timelineMode gui model

deleteFocused
  :: ( Application t m sig
     , Carrier sig m
     , TimelineEffects sig
     , r ~ (n .== Window (t m) (Event TimelineMode))
     )
  => Name n
  -> TimelineModel
  -> t m r r TimelineModeResult
deleteFocused gui model =
  case delete (model ^. currentFocus) (currentProject model ^. timeline) of
    Nothing -> beep gui >> continueWithStatusMessage "Delete failed."
    Just (DeletionResult timeline' deleted (Just cmd)) ->
      case
          modifyFocus (currentProject model ^. timeline)
                      cmd
                      (model ^. currentFocus)
        of
          Left err -> do
            beep gui
            ilift (logLnText Error ("Deleting failed: " <> show err))
            continueWithStatusMessage "Delete failed."
          Right newFocus ->
            model
              &  existingProject
              .  projectHistory
              %~ edit (timeline .~ timeline')
              &  currentFocus
              .~ newFocus
              &  clipboard
              ?~ deleted
              &  timelineMode gui
    Just (DeletionResult timeline' deleted Nothing) ->
      model
        &  existingProject
        .  projectHistory
        %~ edit (timeline .~ timeline')
        &  clipboard
        ?~ deleted
        &  timelineMode gui
  where
    continueWithStatusMessage msg =
      model & statusMessage ?~ msg & timelineMode gui
