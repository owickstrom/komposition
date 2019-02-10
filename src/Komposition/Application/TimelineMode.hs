{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLabels      #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE RebindableSyntax      #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE StandaloneDeriving    #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TupleSections         #-}
{-# LANGUAGE TypeOperators         #-}
module Komposition.Application.TimelineMode where

import           Komposition.Application.Base
import qualified Prelude

import           Control.Effect                                      (Member)
import           Control.Effect.Carrier                              (Carrier)
import           Control.Lens
import qualified Data.List.NonEmpty                                  as NonEmpty
import           Data.Row.Records                                    hiding
                                                                      (split)
import           Data.String                                         (fromString)
import qualified Pipes
import           System.FilePath                                     ((</>))

import           Komposition.Application.Form
import           Komposition.Composition
import           Komposition.Composition.Delete
import           Komposition.Composition.Insert
import           Komposition.Composition.Paste
import           Komposition.Composition.Split
import           Komposition.Duration
import           Komposition.Focus
import           Komposition.Import.Audio
import           Komposition.Import.Video
import           Komposition.Library
import           Komposition.MediaType
import           Komposition.Progress
import           Komposition.Project
import           Komposition.Project.Store
import           Komposition.Render
import qualified Komposition.Render.Composition                      as Render
import qualified Komposition.Render.FFmpeg                           as FFmpeg
import           Komposition.UndoRedo                                (History,
                                                                      current,
                                                                      redo,
                                                                      runAndRecord,
                                                                      undo)
import           Komposition.UserInterface                           hiding (TimelineViewModel (..),
                                                                      previewImagePath,
                                                                      project,
                                                                      statusMessage,
                                                                      zoomLevel)
import qualified Komposition.UserInterface                           as UI
import           Komposition.UserInterface.Dialog
import           Komposition.UserInterface.Help
import           Komposition.VideoSettings

import           Komposition.Application.ImportMode
import           Komposition.Application.KeyMaps
import           Komposition.Application.LibraryMode
import           Komposition.Application.TimelineMode.UndoableAction

type TimelineEffects sig =
  ( Member ProjectStore sig
  , Member VideoImport sig
  , Member AudioImport sig
  , Member Render sig
  )

data TimelineState = TimelineState
  { _history          :: History UndoableAction UndoableState
  , _clipboard        :: Maybe (Insertion ())
  , _statusMessage    :: Maybe Text
  , _zoomLevel        :: ZoomLevel
  , _previewImagePath :: Maybe FilePath
  }
  deriving (Eq, Show)

makeLenses ''TimelineState

data TimelineModeResult
  = TimelineExit TimelineState
  | TimelineClose

timelineViewFromState
  :: UserInterfaceMarkup markup
  => TimelineState
  -> markup (Event 'TimelineMode)
timelineViewFromState state' =
  timelineView $
  UI.TimelineViewModel
  (state' ^. history . current . existingProject . project)
  (state' ^. history . current . timelineFocus)
  (state' ^. statusMessage)
  (state' ^. zoomLevel)
  (state' ^. previewImagePath)

timelineMode
  :: ( Application t m sig
     , TimelineEffects sig
     , Carrier sig m
     , r ~ (n .== Window (t m) (Event 'TimelineMode))
     )
  => Name n
  -> TimelineState
  -> t m r r TimelineModeResult
timelineMode gui state' = do
  patchWindow gui (timelineViewFromState state')
  nextEventOrTimeout gui 5 >>= maybe resetStatusMessage onNextEvent
  where
    continue = timelineMode gui state'
    continueWithStatusMessage msg =
      state' & statusMessage ?~ msg & timelineMode gui
    resetStatusMessage = state' & statusMessage .~ Nothing & timelineMode gui
    onNextEvent        = \case
      CommandKeyMappedEvent (FocusCommand cmd) ->
        case
            modifyFocus (state' ^. history . current . existingProject . project . timeline)
                        cmd
                        (state' ^. history . current . timelineFocus)
          of
            Left err -> do
              beep gui
              printUnexpectedFocusError err cmd
              continue
            Right newFocus ->
              state'
                & history . current . timelineFocus .~ newFocus
                & refreshPreviewAndContinue gui
      CommandKeyMappedEvent (JumpFocus newFocus) ->
        case atFocus newFocus (state' ^. history . current . existingProject . project . timeline) of
          Just _ -> refreshPreviewAndContinue gui (state' & history . current . timelineFocus .~ newFocus)
          Nothing ->
            beep gui >>> continueWithStatusMessage "Couldn't set focus."
      CommandKeyMappedEvent (InsertCommand type' position) ->
        insertIntoTimeline gui state' type' position
      CommandKeyMappedEvent Delete -> -- trace ("Delete" :: Text) $
        case state' & clipboard .~ (insertionFromSomeComposition =<< atFocus currentFocus' currentTimeline)
                    & history %%~ runAndRecord (DeleteAction currentFocus' (DeletionOf 1)) of
          Left err      ->
            beep gui >>> continueWithStatusMessage err
          Right state'' -> refreshPreviewAndContinue gui state''
        where
          currentFocus' = state' ^. history . current . timelineFocus
          currentTimeline = state' ^. history . current . existingProject . project . timeline
      CommandKeyMappedEvent Copy ->
        state'
          &  clipboard .~ (insertionFromSomeComposition =<< atFocus currentFocus' currentTimeline)
          &  timelineMode gui
        where
          currentFocus' = state' ^. history . current . timelineFocus
          currentTimeline = state' ^. history . current . existingProject . project . timeline
      CommandKeyMappedEvent (Paste pos) ->  -- trace ("Paste" :: Text) $
        case state' ^. clipboard of
          Nothing -> beep gui >>> continue
          Just clipboardInsertion ->
            case state' & history %%~ runAndRecord (InsertAction currentFocus' insertPos clipboardInsertion) of
              Left err      ->
                beep gui >>> continueWithStatusMessage err
              Right state'' -> refreshPreviewAndContinue gui state''
            where
              currentFocus' = state' ^. history . current . timelineFocus
              insertPos = case pos of
                PasteLeftOf  -> LeftOf
                PasteRightOf -> RightOf
      CommandKeyMappedEvent Split ->
        case split ( state' ^. history . current . timelineFocus) (state' ^. history . current . existingProject . project . timeline) of
          Just (timeline', newFocus) ->
            state'
              &  history . current .  existingProject . project . timeline .~ timeline'
              &  history . current . timelineFocus .~ newFocus
              &  refreshPreviewAndContinue gui
          Nothing -> do
            beep gui
            continueWithStatusMessage
              "Can't split composition at current focus."
      CommandKeyMappedEvent Import ->
        selectFileToImport >>>= addImportedAssetsToLibrary gui state'
      CommandKeyMappedEvent Render ->
        case Render.flattenTimeline (state' ^. history . current . existingProject . project . timeline) of
          Just flat -> do
            outDir <- ilift getDefaultProjectsDirectory
            chooseFile gui (Save File) "Render To File" outDir >>>= \case
              Just outFile -> do
                stream <- ilift $ renderComposition
                  (state' ^. history . current . existingProject . project . videoSettings . renderVideoSettings)
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
        previewFocusedComposition gui state' >>> continue
      CommandKeyMappedEvent Undo -> -- trace ("Undo" :: Text) $
        case undo (state' ^. history) of
          Just (Left err) -> beep gui >> continueWithStatusMessage err
          Just (Right history') ->
            state'
              & history .~ history'
              & refreshPreviewAndContinue gui
          Nothing -> -- trace ("Can't undo" :: Text) $
            beep gui >> timelineMode gui state'
      CommandKeyMappedEvent Redo ->
        case redo (state' ^. history) of
          Just (Left err) -> beep gui >> continueWithStatusMessage err
          Just (Right history') ->
            state'
              & history .~ history'
              & refreshPreviewAndContinue gui
          Nothing -> beep gui >> timelineMode gui state'
      CommandKeyMappedEvent SaveProject ->
        state' ^. history . current . existingProject
          & saveExistingProject
          & ilift
          & (>> continue)
      CommandKeyMappedEvent CloseProject -> ireturn TimelineClose
      CommandKeyMappedEvent Cancel       -> continue
      CommandKeyMappedEvent Help ->
        help gui [ModeKeyMap STimelineMode (keymaps STimelineMode)] >>>= \case
          Just HelpClosed -> continue
          Nothing         -> continue
      CommandKeyMappedEvent Exit -> ireturn (TimelineExit state')
      ZoomLevelChanged      zl   -> state' & zoomLevel .~ zl & timelineMode gui
      PreviewImageRefreshed p -> state' & previewImagePath .~ p & timelineMode gui
      FocusedClipSpeedSet speed ->
        state'
        & modifyFocusedVideoPart (\case
          VideoClip ann asset ts _ -> VideoClip ann asset ts speed
          vg@VideoGap{} -> vg)
        & refreshPreviewAndContinue gui
      FocusedClipStartSet start ->
        state'
        & modifyFocusedVideoPart (\case
          VideoClip ann asset ts speed ->
                VideoClip ann asset ts { spanStart = start } speed
          vg@VideoGap{} -> vg)
        & refreshPreviewAndContinue gui
      FocusedClipEndSet end ->
        state'
        & modifyFocusedVideoPart (\case
          VideoClip ann asset ts speed ->
                VideoClip ann asset ts { spanEnd = end } speed
          vg@VideoGap{} -> vg)
        & refreshPreviewAndContinue gui
      WindowClosed               -> ireturn (TimelineExit state')

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
     , r ~ (n .== Window (t m) (Event 'TimelineMode))
     )
  => Name n
  -> TimelineState
  -> InsertType
  -> InsertPosition
  -> t m r r TimelineModeResult
insertIntoTimeline gui state' type' position =
  case
      ( type'
      , atFocus ( state' ^. history . current . timelineFocus)
                (state' ^. history . current . existingProject . project . timeline)
      )
    of
      (InsertComposition, Just (SomeSequence _)) ->
         case state' & history %%~ runAndRecord (InsertAction currentFocus' position (InsertSequence emptySequence)) of
           Left err      -> do
             beep gui
             state'
               & statusMessage ?~ err
               & timelineMode gui
           Right state'' -> refreshPreviewAndContinue gui state''
         where
           currentFocus' = state' ^. history . current . timelineFocus
           emptySequence = Sequence () (pure (Parallel () mempty mempty))
      (InsertClip (Just mt), Just SomeParallel{}) -> case mt of
        Video -> selectAssetAndInsert gui state' SVideo position
        Audio -> selectAssetAndInsert gui state' SAudio position
      (InsertClip Nothing, Just SomeVideoPart{}) ->
        selectAssetAndInsert gui state' SVideo position
      (InsertClip Nothing, Just SomeAudioPart{}) ->
        selectAssetAndInsert gui state' SAudio position
      (InsertGap (Just mt), Just SomeParallel{}) -> case mt of
        Video ->
          insertGap gui state' SVideo position >>>= refreshPreviewAndContinue gui
        Audio ->
          insertGap gui state' SAudio position >>>= refreshPreviewAndContinue gui
      (InsertGap Nothing, Just SomeVideoPart{}) ->
        insertGap gui state' SVideo position >>>= refreshPreviewAndContinue gui
      (InsertGap Nothing, Just SomeAudioPart{}) ->
        insertGap gui state' SAudio position >>>= refreshPreviewAndContinue gui
      (c, Just f) -> do
        let
          msg =
            "Cannot perform "
              <> show c
              <> " when focused at "
              <> prettyFocusedAt f
        timelineMode gui (state' & statusMessage ?~ msg)
      (_, Nothing) -> do
        ilift (logLnText Warning "Focus is invalid.")
        continue
  where continue = timelineMode gui state'

insertGap
  :: ( Application t m sig
     , HasType parent (Window (t m) parentEvent) r
     , Typeable parentEvent
     )
  => Name parent
  -> TimelineState
  -> SMediaType mt
  -> InsertPosition
  -> t m r r TimelineState
insertGap parent state' mediaType' position = do
  gapDuration <- prompt parent
                        "Insert Gap"
                        "Please specify a gap duration in seconds."
                        "Insert Gap"
                        (PromptNumber (0.1, 10e10, 0.1))
  let gapInsertion seconds = case mediaType' of
        SVideo -> InsertVideoParts (pure (VideoGap () (durationFromSeconds seconds)))
        SAudio -> InsertAudioParts (pure (AudioGap () (durationFromSeconds seconds)))
  case gapDuration of
    Just seconds ->
      case state' & history %%~ runAndRecord (InsertAction currentFocus' position (gapInsertion seconds)) of
        Left err      -> do
          beep parent
          state'
            & statusMessage ?~ err
            & ireturn
        Right state'' -> ireturn state''
      where
        currentFocus' = state' ^. history . current . timelineFocus
    Nothing -> ireturn state'

prettyFocusedAt :: FocusedAt a -> Text
prettyFocusedAt = \case
  SomeSequence{}  -> "sequence"
  SomeParallel{}  -> "parallel"
  SomeVideoTrack{} -> "video track"
  SomeAudioTrack{} -> "audio track"
  SomeVideoPart{} -> "video part"
  SomeAudioPart{} -> "audio part"

previewFocusedComposition
  :: ( Application t m sig
     , HasType n (Window (t m) e) r
     , Carrier sig m
     , TimelineEffects sig
     , Typeable e
     )
  => Name n
  -> TimelineState
  -> t m r r TimelineState
previewFocusedComposition gui state' =
  case atFocus ( state' ^. history . current . timelineFocus) (state' ^. history . current . existingProject . project . timeline) of
    Just (SomeSequence s) -> renderFlatComposition (Render.flattenSequence s)
    Just (SomeParallel p) -> renderFlatComposition (Render.flattenParallel p)
    Just (SomeVideoTrack t) -> renderFlatComposition (Render.flattenParallel (Parallel () t mempty))
    Just (SomeAudioTrack t) -> renderFlatComposition (Render.flattenParallel (Parallel () mempty t))
    Just (SomeVideoPart p) -> renderFlatComposition (Render.singleVideoPart p)
    Just (SomeAudioPart (AudioClip _ asset)) ->
      previewFile (asset ^. assetMetadata . path . unOriginalPath)
    Just (SomeAudioPart AudioGap{}) -> beepWith "Can't preview audio gap."
    Nothing -> beepWith "Can't preview when no timeline part is focused."
  where
    renderFlatComposition = \case
      Just flat -> do
        streamingProcess <- ilift $ renderComposition
          (state' ^. history . current . existingProject . project . videoSettings . proxyVideoSettings)
          VideoProxy
          (HttpStreamingOutput "localhost" 12345)
          flat
        _ <- previewStream
          gui
          "http://localhost:12345"
          streamingProcess
          (state' ^. history . current . existingProject . project . videoSettings . proxyVideoSettings)
        ireturn state'
      Nothing -> beepWith "Cannot preview a composition without video clips."
    previewFile fp = do
      _ <- previewStream
        gui
        ("file://" <> toS fp)
        (Pipes.yield (ProgressUpdate "Loading clip" 1))
        (state' ^. history . current . existingProject . project . videoSettings . proxyVideoSettings)
      ireturn state'
    beepWith msg = do
      beep gui
      state' & statusMessage ?~ msg & ireturn

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
     , r ~ (n .== Window (t m) (Event 'TimelineMode))
     )
  => Name n
  -> TimelineState
  -> SMediaType mt
  -> InsertPosition
  -> t m r r TimelineModeResult
selectAssetAndInsert gui state' mediaType' position = case mediaType' of
  SVideo ->
    case NonEmpty.nonEmpty (state' ^. history . current . existingProject . project . library . videoAssets) of
      Just vs -> do
        selected <- selectAsset (SelectAssetsModel SVideo vs [])
        case NonEmpty.nonEmpty Prelude.=<< selected of
          Just assets -> insertSelectedAssets gui state' SVideo position assets
          Nothing -> beep gui >>> timelineMode gui state'
      Nothing -> onNoAssets gui SVideo
  SAudio ->
    case NonEmpty.nonEmpty (state' ^. history . current . existingProject . project . library . audioAssets) of
      Just as -> do
        selected <- selectAsset (SelectAssetsModel SAudio as [])
        case NonEmpty.nonEmpty Prelude.=<< selected of
          Just assets -> insertSelectedAssets gui state' SAudio position assets
          Nothing -> beep gui >>> timelineMode gui state'
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
      state' & statusMessage ?~ noAssetsMessage mt & timelineMode gui'

insertSelectedAssets
  :: ( Application t m sig
     , Carrier sig m
     , TimelineEffects sig
     , r ~ (n .== Window (t m) (Event 'TimelineMode))
     )
  => Name n
  -> TimelineState
  -> SMediaType mt
  -> InsertPosition
  -> NonEmpty (Asset mt)
  -> t m r r TimelineModeResult
insertSelectedAssets gui state' mediaType' position assets =
  case state' & history %%~ runAndRecord (InsertAction currentFocus' position (insertionOf mediaType' assets)) of
    Left err      -> do
      beep gui
      state'
        & statusMessage ?~ err
        & refreshPreviewAndContinue gui
    Right state'' -> timelineMode gui state''
  where
    currentFocus' = state'^.history.current.timelineFocus

insertionOf
  :: SMediaType mt
  -> NonEmpty (Asset mt)
  -> Insertion ()
insertionOf SVideo a = InsertVideoParts (toVideoClip <$> a)
  where
    toVideoClip videoAsset =
      let ts = maybe (TimeSpan 0 (durationOf OriginalDuration videoAsset))
                     snd
                     (videoAsset ^. videoClassifiedScene)
          speed = videoAsset ^. videoSpeed
      in  VideoClip () videoAsset ts speed
insertionOf SAudio a     = InsertAudioParts (AudioClip () <$> a)

addImportedAssetsToLibrary
  :: ( Application t m sig
     , Carrier sig m
     , TimelineEffects sig
     , r ~ (n .== Window (t m) (Event 'TimelineMode))
     )
  => Name n
  -> TimelineState
  -> Maybe (ImportFileForm Valid)
  -> t m r r TimelineModeResult
addImportedAssetsToLibrary gui state' (Just selected) = do
  state'' <-
    importSelectedFile gui (state' ^. history . current . existingProject) selected >>>= \case
      Just (Left err) -> do
        ilift (logLnShow Error err)
        _ <- dialog
          gui
          DialogProperties
            { dialogTitle   = "Import Failed!"
            , dialogMessage = show err
            , dialogChoices = [Ok]
            }
        ireturn state'
      Just (Right (Left vs)) ->
        state'
          &  history . current . existingProject . project . library . videoAssets %~ (<> vs)
          &  ireturn
      Just (Right (Right as)) ->
        state'
          &  history . current . existingProject . project . library . audioAssets %~ (<> as)
          &  ireturn
      Nothing -> ireturn state'
  timelineMode gui state''
addImportedAssetsToLibrary gui state' Nothing = timelineMode gui state'

refreshPreview
  :: ( Application t m sig
     , Carrier sig m
     , TimelineEffects sig
     , r ~ (n .== Window (t m) (Event 'TimelineMode))
     )
  => Name n
  -> TimelineState
  -> t m r r ()
refreshPreview gui state' = do
  cacheDir <- ilift getCacheDirectory
  case atFocus ( state' ^. history . current . timelineFocus) (state' ^. history . current . existingProject . project . timeline) of
    Just (SomeVideoPart (VideoClip _ videoAsset ts _)) ->
      runInBackground gui $
        pure . PreviewImageRefreshed . Just <$>
        FFmpeg.extractFrameToFile'
          (state' ^. history . current . existingProject . project . videoSettings . proxyVideoSettings)
          Render.FirstFrame
          VideoProxy
          videoAsset
          ts
          (cacheDir </> "preview-frame")
    _ -> runInBackground gui (pure (pure (PreviewImageRefreshed Nothing)))

refreshPreviewAndContinue
  :: ( Application t m sig
     , Carrier sig m
     , TimelineEffects sig
     , r ~ (n .== Window (t m) (Event 'TimelineMode))
     )
  => Name n
  -> TimelineState
  -> t m r r TimelineModeResult
refreshPreviewAndContinue gui state' = do
  refreshPreview gui state'
  timelineMode gui state'

modifyFocusedVideoPart
  :: (VideoPart () -> VideoPart ()) -> TimelineState -> TimelineState
modifyFocusedVideoPart f state' =
  state'
    &  history
    .  current
    .  existingProject
    .  project
    .  timeline
    .  focusing (state' ^. history . current . timelineFocus)
    %~ f
