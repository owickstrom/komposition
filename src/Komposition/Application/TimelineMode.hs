{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns        #-}
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

import           Komposition.Application.Base        hiding (to)
import qualified Prelude

import           Control.Effect                      (Member)
import           Control.Effect.Carrier              (Carrier)
import           Control.Lens                        hiding (preview)
import qualified Data.List.NonEmpty                  as NonEmpty
import           Data.Row.Records                    hiding (split)
import           Data.String                         (fromString)
import           Pipes                               ((>->))
import qualified Pipes
import qualified Pipes.Safe                          as Pipes (runSafeT, tryP)
import           System.FilePath                     ((</>))

import           Komposition.Application.Form
import           Komposition.Composition
import           Komposition.Composition.Delete
import           Komposition.Composition.Insert
import           Komposition.Composition.Paste
import qualified Komposition.Composition.Split       as Split
import           Komposition.Duration
import qualified Komposition.FFmpeg.Process          as FFmpeg
import           Komposition.Focus
import           Komposition.Import.Audio
import           Komposition.Import.Video
import           Komposition.Library
import           Komposition.MediaType
import           Komposition.Project
import           Komposition.Project.Store
import           Komposition.Project.UndoableAction
import           Komposition.Render
import qualified Komposition.Render.Composition      as Render
import qualified Komposition.Render.FFmpeg           as FFmpeg
import           Komposition.UndoRedo                (current, redo,
                                                      runAndRecord, undo)
import           Komposition.UserInterface           hiding
                                                      (TimelineViewModel (..),
                                                      preview, project,
                                                      statusMessage, zoomLevel)
import qualified Komposition.UserInterface           as UI
import           Komposition.UserInterface.WindowUserInterface
import           Komposition.UserInterface.Dialog
import           Komposition.UserInterface.Help
import           Komposition.VideoSettings

import           Komposition.Application.ImportMode
import           Komposition.Application.KeyMaps
import           Komposition.Application.LibraryMode
import qualified Komposition.KeyMap                  as KeyMap

type TimelineEffects sig =
  ( Member ProjectStore sig
  , Member VideoImport sig
  , Member AudioImport sig
  , Member Render sig
  )

data PreviewingState t m = PreviewingState
  { _previewType     :: Preview
  , _previewProcess  :: Maybe (BackgroundProcess (t m))
  , _previewProgress :: Maybe Double
  }

makeLenses ''PreviewingState

instance Show (PreviewingState t m) where
  show state' =
       "{ _previewType     = " <> show (state' ^. previewType)
    <> ", _previewProcess  = " <> show (state' ^. previewProcess . _Just . to (const ("<process>" :: Prelude.String)))
    <> ", _previewProgress = " <> show (state' ^. previewProgress)
    <> "}"

data PreviewState t m
  = Previewing (PreviewingState t m)
  | NotPreviewing
  deriving (Show)

makePrisms ''PreviewState


data TimelineState t m = TimelineState
  { _existingProject :: WithHistory ExistingProject
  , _clipboard       :: Maybe (Insertion ())
  , _statusMessage   :: Maybe Text
  , _zoomLevel       :: ZoomLevel
  , _preview         :: PreviewState t m
  } deriving (Show)

makeLenses ''TimelineState

-- instance Show (TimelineState t m) where
--   show state =
--        "{ _existingProject = " <> show (state ^. existingProject)
--     <> ", _clipboard = " <> show (state ^. clipboard)
--     <> ", _statusMessage = " <> show (state ^. statusMessage)
--     <> ", _zoomLevel = " <> show (state ^. zoomLevel)
--     <> ", _preview = " <> show (state ^. preview)
--     <> "} "

data TimelineModeResult t m
  = TimelineExit (TimelineState t m)
  | TimelineClose

timelineViewFromState
  :: UserInterfaceMarkup markup
  => TimelineState t m
  -> markup 'TopWindow (Event 'TimelineMode)
timelineViewFromState state' =
  timelineView $
  UI.TimelineViewModel
  (state' ^. existingProject.project)
  (state' ^. existingProject.project.timelineFocus)
  (state' ^. statusMessage)
  (state' ^. zoomLevel)
  (state' ^? preview . _Previewing . previewType)
  (state' ^?  preview . _Previewing . previewProgress . traverse)

timelineMode
  :: ( Application t m sig
     , TimelineEffects sig
     , Carrier sig m
     , r ~ (n .== Window (t m) 'TopWindow (Event 'TimelineMode))
     )
  => Name n
  -> TimelineState t m
  -> t m r r (TimelineModeResult t m)
timelineMode gui state' = do
  patchWindow gui (timelineViewFromState state')
  nextEventOrTimeout gui 5 >>= maybe resetStatusMessage onNextEvent
  where
    continue = timelineMode gui state'
    continueWithStatusMessage msg =
      state' & statusMessage ?~ msg & timelineMode gui
    resetStatusMessage = state' & statusMessage .~ Nothing & timelineMode gui
    onNextEvent        event = case event of
      CommandKeyMappedEvent (FocusCommand cmd) ->
        case
            modifyFocus (state' ^. existingProject.project.timeline.current)
                        cmd
                        (state' ^. existingProject.project.timelineFocus)
          of
            Left err -> do
              beep gui
              printUnexpectedFocusError err cmd
              continue
            Right newFocus ->
              state'
                & existingProject.project.timelineFocus .~ newFocus
                & refreshPreviewAndContinue gui
      CommandKeyMappedEvent (JumpFocus newFocus) ->
        case atFocus newFocus (state' ^. existingProject.project.timeline.current) of
          Just _ -> refreshPreviewAndContinue gui (state' & existingProject.project.timelineFocus .~ newFocus)
          Nothing ->
            beep gui >>> continueWithStatusMessage "Couldn't set focus."
      CommandKeyMappedEvent (InsertCommand type' position) ->
        insertIntoTimeline gui state' type' position
      CommandKeyMappedEvent Delete ->
        state'
          & clipboard .~ clipboardInsertion
          & runUndoableAction gui (DeleteAction currentFocus' (DeletionOf 1))
        where
          currentFocus' = state' ^. existingProject.project.timelineFocus
          currentTimeline = state' ^. existingProject.project.timeline.current
          clipboardInsertion = insertionFromSomeComposition =<< atFocus currentFocus' currentTimeline
      CommandKeyMappedEvent Copy ->
        state'
          &  clipboard .~ (insertionFromSomeComposition =<< atFocus currentFocus' currentTimeline)
          &  timelineMode gui
        where
          currentFocus' = state' ^. existingProject.project.timelineFocus
          currentTimeline = state' ^. existingProject.project.timeline.current
      CommandKeyMappedEvent (Paste pos) ->
        case state' ^. clipboard of
          Nothing -> beep gui >>> continue
          Just clipboardInsertion ->
            runUndoableAction gui (InsertAction currentFocus' insertPos clipboardInsertion) state'
            where
              currentFocus' = state' ^. existingProject.project.timelineFocus
              insertPos = case pos of
                PasteLeftOf  -> LeftOf
                PasteRightOf -> RightOf
      CommandKeyMappedEvent Split ->
        runUndoableAction gui (SplitAction currentFocus' Split.OnClipsNearFocus) state'
          where
            currentFocus' = state' ^. existingProject.project.timelineFocus
      CommandKeyMappedEvent Join ->
        runUndoableAction gui (JoinAction currentFocus') state'
          where
            currentFocus' = state' ^. existingProject.project.timelineFocus
      CommandKeyMappedEvent Import ->
        selectFileToImport >>>= addImportedAssetsToLibrary gui state'
      CommandKeyMappedEvent Render ->
        case Render.flattenTimeline (state' ^. existingProject.project.timeline.current) of
          Just flat -> do
            outDir <- ilift getDefaultProjectsDirectory
            chooseFile gui (Save File) "Render To File" outDir >>>= \case
              Just outFile -> do
                stream <- ilift $ renderComposition
                  (state' ^. existingProject . project . videoSettings . renderVideoSettings)
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
      CommandKeyMappedEvent PlayOrStop ->
        playFocusedComposition gui state' >>>= refreshPreviewAndContinue gui
      CommandKeyMappedEvent Undo ->
        case undo (state' ^. existingProject.project.timeline) of
          Just (Left err) -> beep gui >> continueWithStatusMessage err
          Just (Right (newHistory, newFocus)) ->
            state'
              & existingProject.project.timeline .~ newHistory
              & existingProject.project.timelineFocus .~ newFocus
              & refreshPreviewAndContinue gui
          Nothing ->
            beep gui >> timelineMode gui state'
      CommandKeyMappedEvent Redo ->
        case redo (state' ^. existingProject.project.timeline) of
          Just (Left err) -> beep gui >> continueWithStatusMessage err
          Just (Right (newHistory, newFocus)) ->
            state'
              & existingProject.project.timeline .~ newHistory
              & existingProject.project.timelineFocus .~ newFocus
              & refreshPreviewAndContinue gui
          Nothing -> beep gui >> timelineMode gui state'
      CommandKeyMappedEvent SaveProject ->
        state' ^. existingProject
          & dropHistory
          & saveExistingProject
          & ilift
          & (>> continue)
      CommandKeyMappedEvent CloseProject -> ireturn TimelineClose
      CommandKeyMappedEvent Cancel       -> continue
      CommandKeyMappedEvent Help ->
        help gui [ModeKeyMap STimelineMode (keymaps STimelineMode)] >>>= \case
          Just HelpClosed -> continue
          Nothing         -> continue
      CommandKeyMappedEvent Exit ->
        ireturn (TimelineExit state')
      ZoomLevelChanged      zl   -> state' & zoomLevel .~ zl & timelineMode gui
      PreviewFrameExtracted path' ->
        state'
        & preview .~ Previewing (PreviewingState (PreviewFrame path') Nothing Nothing)
        & timelineMode gui
      FocusedClipSpeedSet speed -> runUndoableAction gui (SetClipSpeed (state' ^. existingProject.project.timelineFocus) speed) state'
      FocusedClipStartSet start -> runUndoableAction gui (SetClipStart (state' ^. existingProject.project.timelineFocus)start) state'
      FocusedClipEndSet end -> runUndoableAction gui (SetClipEnd (state' ^. existingProject.project.timelineFocus)end) state'
      StreamingProcessFailed msg -> do
        ilift (logLnText Error ("Streaming process failed: " <> msg))
        state'
          & preview .~ NotPreviewing
          & refreshPreviewAndContinue gui
      PlaybackProgress{} -> continue
      PlaybackRestarting -> continue
      PlaybackFinished -> continue
      WindowClosed -> ireturn (TimelineExit state')

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
     , r ~ (n .== Window (t m) 'TopWindow (Event 'TimelineMode))
     )
  => Name n
  -> TimelineState t m
  -> InsertType
  -> InsertPosition
  -> t m r r (TimelineModeResult t m)
insertIntoTimeline gui state' type' position =
  case
      ( type'
      , atFocus ( state' ^. existingProject.project.timelineFocus)
                (state' ^. existingProject.project.timeline.current)
      )
    of
      (InsertComposition, Just (SomeSequence _)) -> insertComposition (InsertSequence emptySequence)
      (InsertComposition, Just (SomeParallel _)) -> insertComposition (InsertParallel emptyParallel)
      (InsertClip (Just mt), Just SomeParallel{}) -> case mt of
        Video -> selectAssetAndInsert gui state' SVideo position
        Audio -> selectAssetAndInsert gui state' SAudio position
      (InsertClip Nothing, Just SomeVideoTrack{}) -> insertVideoClip
      (InsertClip (Just Video), Just SomeVideoTrack{}) -> insertVideoClip
      (InsertClip Nothing, Just SomeAudioTrack{}) -> insertAudioClip
      (InsertClip (Just Audio), Just SomeAudioTrack{}) -> insertAudioClip
      (InsertClip Nothing, Just SomeVideoPart{}) -> insertVideoClip
      (InsertClip (Just Video), Just SomeVideoPart{}) -> insertVideoClip
      (InsertClip Nothing, Just SomeAudioPart{}) -> insertAudioClip
      (InsertClip (Just Audio), Just SomeAudioPart{}) -> insertAudioClip
      (InsertGap (Just mt), Just SomeParallel{}) -> case mt of
        Video -> insertVideoGap
        Audio -> insertAudioGap
      (InsertGap Nothing, Just SomeVideoTrack{}) -> insertVideoGap
      (InsertGap (Just Video), Just SomeVideoTrack{}) -> insertVideoGap
      (InsertGap Nothing, Just SomeAudioTrack{}) -> insertAudioGap
      (InsertGap (Just Audio), Just SomeAudioTrack{}) -> insertAudioGap
      (InsertGap Nothing, Just SomeVideoPart{}) -> insertVideoGap
      (InsertGap (Just Video), Just SomeVideoPart{}) -> insertVideoGap
      (InsertGap Nothing, Just SomeAudioPart{}) -> insertAudioGap
      (InsertGap (Just Audio), Just SomeAudioPart{}) -> insertAudioGap
      (insertType, Just f) -> do
        let
          insertTypePretty = \case
            InsertComposition -> "insert composition"
            InsertClip mt -> "insert" <> mediaTypePretty mt <> " clip"
            InsertGap mt -> "insert" <> mediaTypePretty mt <> " gap"
          mediaTypePretty = \case
            Just Video -> " video"
            Just Audio -> " audio"
            Nothing -> mempty
          msg =
            "Cannot "
              <> insertTypePretty insertType
              <> " when focused at "
              <> prettyFocusedAt f
        timelineMode gui (state' & statusMessage ?~ msg)
      (_, Nothing) -> do
        ilift (logLnText Warning "Focus is invalid.")
        continue
  where
    continue = timelineMode gui state'
    insertVideoClip = selectAssetAndInsert gui state' SVideo position
    insertAudioClip = selectAssetAndInsert gui state' SAudio position
    insertVideoGap = insertGap gui state' SVideo position >>>= refreshPreviewAndContinue gui
    insertAudioGap = insertGap gui state' SAudio position >>>= refreshPreviewAndContinue gui
    insertComposition insertion =
         case runAndRecord (InsertAction currentFocus' position insertion) (state' ^. existingProject.project.timeline) of
           Left err      -> do
             beep gui
             state'
               & statusMessage ?~ err
               & timelineMode gui
           Right (newHistory, newFocus) ->
             state'
               & existingProject.project.timeline .~ newHistory
               & existingProject.project.timelineFocus .~ newFocus
               & refreshPreviewAndContinue gui
         where
           currentFocus' = state' ^. existingProject.project.timelineFocus
    emptySequence = Sequence () (pure emptyParallel)
    emptyParallel = Parallel () mempty mempty

insertGap
  :: ( Application t m sig
     , HasType parent (Window (t m) 'TopWindow parentEvent) r
     , Typeable parentEvent
     )
  => Name parent
  -> TimelineState t m
  -> SMediaType mt
  -> InsertPosition
  -> t m r r (TimelineState t m)
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
      case runAndRecord (InsertAction currentFocus' position (gapInsertion seconds)) (state' ^. existingProject.project.timeline) of
        Left err      -> do
          beep parent
          state'
            & statusMessage ?~ err
            & ireturn
        Right (newHistory, newFocus) ->
          state'
            & existingProject.project.timeline .~ newHistory
            & existingProject.project.timelineFocus .~ newFocus
            & ireturn
      where
        currentFocus' = state' ^. existingProject.project.timelineFocus
    Nothing -> ireturn state'

prettyFocusedAt :: FocusedAt a -> Text
prettyFocusedAt = \case
  SomeSequence{}  -> "sequence"
  SomeParallel{}  -> "parallel"
  SomeVideoTrack{} -> "video track"
  SomeAudioTrack{} -> "audio track"
  SomeVideoPart{} -> "video part"
  SomeAudioPart{} -> "audio part"

playFocusedComposition
  :: ( Application t m sig
     , r ~ (n .== Window (t m) 'TopWindow (Event 'TimelineMode))
     , Carrier sig m
     , TimelineEffects sig
     )
  => Name n
  -> TimelineState t m
  -> t m r r (TimelineState t m)
playFocusedComposition gui state' =
  case
      atFocus (state' ^. existingProject . project . timelineFocus)
              (state' ^. existingProject . project . timeline . current)
    of
      Just (SomeSequence s) -> renderFlatComposition (Render.flattenSequence s)
      Just (SomeParallel p) -> renderFlatComposition (Render.flattenParallel p)
      Just (SomeVideoTrack t) ->
        renderFlatComposition (Render.flattenParallel (Parallel () t mempty))
      Just (SomeAudioTrack t) ->
        renderFlatComposition (Render.flattenParallel (Parallel () mempty t))
      Just (SomeVideoPart p) ->
        renderFlatComposition (Render.singleVideoPart p)
      Just (SomeAudioPart (AudioClip _ asset)) -> playFile
        (asset ^. assetMetadata . path . unOriginalPath)
        (asset ^. assetMetadata . duration)
      Just (SomeAudioPart AudioGap{}) -> beepWith "Can't play audio gap."
      Nothing -> beepWith "Can't play when no timeline part is focused."
  where
    renderFlatComposition = \case
      Just flat -> do
        let host = "localhost"
            port = 12345
        ilift (logLnText Debug "Rendering...")
        streamingProcess <- ilift $ renderComposition
          (  state'
          ^. existingProject
          .  project
          .  videoSettings
          .  proxyVideoSettings
          )
          VideoProxy
          (HttpStreamingOutput host port)
          flat
        let ignoreProgress = forever (void Pipes.await)
        bg <- runInBackground gui $ do
          Pipes.runSafeT
              (Pipes.runEffect (Pipes.tryP streamingProcess >-> ignoreProgress))
            Prelude.>>= \case
                          Left (FFmpeg.ProcessFailed e) ->
                            pure (Just (StreamingProcessFailed e))
                          Right () -> pure Nothing
        ilift (logLnText Debug "Going into playing...")
        state'
          &  setPlayingMessage
          &  preview
          .~ Previewing
               (PreviewingState
                 (PlayHttpStream host port (durationOf AdjustedDuration flat))
                 (Just bg)
                 (Just 0)
               )
          &  inPlaying gui
      Nothing -> beepWith "Cannot play a composition without video clips."
    playFile fp fullDuration =
      state'
        &  setPlayingMessage
        &  preview
        .~ Previewing
             (PreviewingState (PlayFile fp fullDuration) Nothing (Just 0))
        &  inPlaying gui
    setPlayingMessage =
      case KeyMap.lookupSequence PlayOrStop (keymaps STimelineMode) of
        [] -> statusMessage ?~ "Playing..."
        (ks : _) ->
          statusMessage
            ?~ "Playing... (hit "
            <> KeyMap.keySequenceToText ks
            <> " to stop)"
    beepWith msg = do
      beep gui
      state' & statusMessage ?~ msg & ireturn

inPlaying
  :: ( Application t m sig
     , r ~ (n .== Window (t m) 'TopWindow (Event 'TimelineMode))
     , Carrier sig m
     , TimelineEffects sig
     )
  => Name n
  -> TimelineState t m
  -> t m r r (TimelineState t m)
inPlaying gui state' = awaitFinished state' >>>= cleanup >>>= reset
  where
    awaitFinished state'' = do
      patchWindow gui (timelineViewFromState state'')
      nextEvent gui >>>= \case
        StreamingProcessFailed msg -> do
          ilift (logLnText Error ("Streaming process failed during playback: " <> msg))
          ireturn state''
        CommandKeyMappedEvent PlayOrStop -> do
          ilift (logLnText Debug "Playback stopped.")
          ireturn state''
        PlaybackProgress p -> do
          ilift (logLnText Trace ("Playback progress: " <> show p))
          state''
            & preview . _Previewing . previewProgress ?~ p
            & awaitFinished
        PlaybackRestarting -> do
          ilift (logLnText Trace "Playback restarting...")
          awaitFinished state''
        PlaybackFinished -> do
          ilift (logLnText Debug "Playback finished.")
          ireturn state''
        _ -> beep gui >>> awaitFinished state''
    cleanup state'' =
      case state'' ^?! preview . _Previewing . previewProcess of
        Just bg -> do
          ilift (logLnText Debug "Cancelling streaming process...")
          cancelProcess bg
          ilift (logLnText Debug "Cancelled streaming process.")
          ireturn state''
        Nothing              -> ireturn state''
    reset state'' =
      state''
        & preview .~ NotPreviewing
        & statusMessage .~ Nothing
        & ireturn


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
     , r ~ (n .== Window (t m) 'TopWindow (Event 'TimelineMode))
     )
  => Name n
  -> TimelineState t m
  -> SMediaType mt
  -> InsertPosition
  -> t m r r (TimelineModeResult t m)
selectAssetAndInsert gui state' mediaType' position = case mediaType' of
  SVideo ->
    case NonEmpty.nonEmpty (state' ^. existingProject . project . library . videoAssets) of
      Just vs -> do
        selected <- selectAsset (SelectAssetsModel SVideo vs [])
        case NonEmpty.nonEmpty Prelude.=<< selected of
          Just assets -> insertSelectedAssets gui state' SVideo position assets
          Nothing -> beep gui >>> timelineMode gui state'
      Nothing -> do
        beep gui
        state' & statusMessage ?~ noAssetsMessage SVideo & timelineMode gui
  SAudio ->
    case NonEmpty.nonEmpty (state' ^. existingProject . project . library . audioAssets) of
      Just as -> do
        selected <- selectAsset (SelectAssetsModel SAudio as [])
        case NonEmpty.nonEmpty Prelude.=<< selected of
          Just assets -> insertSelectedAssets gui state' SAudio position assets
          Nothing -> beep gui >>> timelineMode gui state'
      Nothing -> do
        beep gui
        state' & statusMessage ?~ noAssetsMessage SAudio & timelineMode gui

insertSelectedAssets
  :: ( Application t m sig
     , Carrier sig m
     , TimelineEffects sig
     , r ~ (n .== Window (t m) 'TopWindow (Event 'TimelineMode))
     )
  => Name n
  -> TimelineState t m
  -> SMediaType mt
  -> InsertPosition
  -> NonEmpty (Asset mt)
  -> t m r r (TimelineModeResult t m)
insertSelectedAssets gui state' mediaType' position assets =
  case runAndRecord (InsertAction currentFocus' position (insertionOf mediaType' assets)) (state' ^. existingProject.project.timeline) of
    Left err      -> do
      beep gui
      state'
        & statusMessage ?~ err
        & refreshPreviewAndContinue gui
    Right (newHistory, newFocus) ->
      state'
        & existingProject.project.timeline .~ newHistory
        & existingProject.project.timelineFocus .~ newFocus
        & timelineMode gui
  where
    currentFocus' = state'^.existingProject.project.timelineFocus

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
     , r ~ (n .== Window (t m) 'TopWindow (Event 'TimelineMode))
     )
  => Name n
  -> TimelineState t m
  -> Maybe (ImportFileForm Valid)
  -> t m r r (TimelineModeResult t m)
addImportedAssetsToLibrary gui state' (Just selected) = do
  state'' <-
    importSelectedFile gui (state' ^. existingProject) selected >>>= \case
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
          &  existingProject . project . library . videoAssets %~ (<> vs)
          &  ireturn
      Just (Right (Right as)) ->
        state'
          &  existingProject . project . library . audioAssets %~ (<> as)
          &  ireturn
      Nothing -> ireturn state'
  timelineMode gui state''
addImportedAssetsToLibrary gui state' Nothing = timelineMode gui state'

refreshPreview
  :: ( Application t m sig
     , Carrier sig m
     , TimelineEffects sig
     , r ~ (n .== Window (t m) 'TopWindow (Event 'TimelineMode))
     )
  => Name n
  -> TimelineState t m
  -> t m r r ()
refreshPreview gui state' = do
  cacheDir <- ilift getCacheDirectory
  case atFocus (state' ^. existingProject.project.timelineFocus) (state' ^. existingProject.project.timeline.current) of
    Just (SomeVideoPart (VideoClip _ videoAsset ts _)) ->
      ivoid . runInBackground gui $
        pure . PreviewFrameExtracted <$>
        FFmpeg.extractFrameToFile'
          (state' ^. existingProject . project . videoSettings . proxyVideoSettings)
          Render.FirstFrame
          VideoProxy
          videoAsset
          ts
          (cacheDir </> "preview-frame")
    _ -> ireturn ()

refreshPreviewAndContinue
  :: ( Application t m sig
     , Carrier sig m
     , TimelineEffects sig
     , r ~ (n .== Window (t m) 'TopWindow (Event 'TimelineMode))
     )
  => Name n
  -> TimelineState t m
  -> t m r r (TimelineModeResult t m)
refreshPreviewAndContinue gui state' = do
  refreshPreview gui state'
  timelineMode gui state'

runUndoableAction
  :: ( Application t m sig
     , Carrier sig m
     , TimelineEffects sig
     , r ~ (n .== Window (t m) 'TopWindow (Event 'TimelineMode))
     )
  => Name n
  -> UndoableAction
  -> TimelineState t m
  -> t m r r (TimelineModeResult t m)
runUndoableAction gui action state' =
  case runAndRecord action (state' ^. existingProject . project . timeline) of
    Left err -> do
      beep gui
      state' & statusMessage ?~ err & timelineMode gui
    Right (newHistory, newFocus) ->
      state'
        & existingProject.project.timeline .~ newHistory
        & existingProject.project.timelineFocus .~ newFocus
        & refreshPreviewAndContinue gui
