{-# OPTIONS_GHC -fno-warn-unticked-promoted-constructors #-}

{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE ExplicitForAll        #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLabels      #-}
{-# LANGUAGE OverloadedLists       #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE RebindableSyntax      #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
module FastCut.Application where

import           FastCut.Prelude             hiding (State, (>>), (>>=))

import           Control.Lens
import           Data.Row.Records
import           Data.String                 (fromString)
import           GHC.Exts                    (fromListN)
import           Motor.FSM                   hiding (Delete, delete)
import           System.Directory
import           Text.Printf

import           Control.Monad.Indexed.IO
import           Control.Monad.Indexed.Trans
import           FastCut.Composition
import           FastCut.Composition.Delete
import           FastCut.Composition.Insert
import           FastCut.Duration
import           FastCut.Focus
import           FastCut.Import.FFmpeg
import           FastCut.KeyMap
import           FastCut.Library
import           FastCut.MediaType
import           FastCut.Project
import qualified FastCut.Render.Composition  as Render
import qualified FastCut.Render.FFmpeg       as Render
import           FastCut.UserInterface

(>>) :: IxMonad m => m i j a -> m j k b -> m i k b
(>>) = (>>>)

(>>=) :: IxMonad m => m i j a -> (a -> m j k b) -> m i k b
(>>=) = (>>>=)

type Application t m
   = ( IxPointed (t m)
     , UserInterface (t m)
     , IxMonadIO (t m)
     , IxMonadTrans t
     , Monad m
     )

keymaps :: SMode m -> KeyMap (Event m)
keymaps = fmap CommandKeyMappedEvent . \case
  STimelineMode ->
    [ ([KeyChar 'h'], Mapping (FocusCommand FocusLeft))
    , ([KeyChar 'j'], Mapping (FocusCommand FocusDown))
    , ([KeyChar 'k'], Mapping (FocusCommand FocusUp))
    , ([KeyChar 'l'], Mapping (FocusCommand FocusRight))
    , ([KeyChar 'i'], Mapping Import)
    , ([KeyChar 'r'], Mapping Render)
    , ( [KeyChar 'a']
      , SequencedMappings
        [ ([KeyChar 'c'], Mapping (AppendCommand AppendClip))
        , ([KeyChar 'g'], Mapping (AppendCommand AppendGap))
        , ([KeyChar 'p'], Mapping (AppendCommand AppendComposition))
        ]
      )
    , ([KeyChar 'd'], Mapping Delete)
    , ([KeyChar 'q'], Mapping Exit)
    ]
  SLibraryMode ->
    [ ([KeyChar 'j'], Mapping LibraryDown)
    , ([KeyChar 'k'], Mapping LibraryUp)
    , ([KeyChar 'q'], Mapping Cancel)
    , ([KeyEnter]   , Mapping LibrarySelect)
    ]
  SImportMode -> [([KeyChar 'q'], Mapping Cancel)]

selectAssetFromList
  :: (UserInterface m, IxMonadIO m, Modify n (State m LibraryMode) r ~ r)
  => Name n
  -> [Asset mt]
  -> Int
  -> Actions
       m
       '[n := Remain (State m LibraryMode)]
       r
       (Maybe (Asset mt))
selectAssetFromList gui assets n = do
  updateLibrary gui assets n
  nextEvent gui >>>= \case
    CommandKeyMappedEvent Cancel        -> ireturn Nothing
    CommandKeyMappedEvent LibrarySelect -> ireturn (assets ^? element n)
    CommandKeyMappedEvent LibraryUp
      | n > 0     -> selectAssetFromList gui assets (pred n)
      | otherwise -> continue
    CommandKeyMappedEvent LibraryDown
      | n < length assets - 1 -> selectAssetFromList gui assets (succ n)
      | otherwise             -> continue
  where continue = selectAssetFromList gui assets n

selectAsset
  :: Application t m
  => Name n
  -> Project
  -> Focus ft
  -> SMediaType mt
  -> ThroughMode
       TimelineMode
       LibraryMode
       (t m)
       n
       (Maybe (Asset mt))
selectAsset gui project focus' mediaType = case mediaType of
  SVideo -> do
    enterLibrary gui (project ^. library . videoAssets) 0
    asset' <- selectAssetFromList gui (project ^. library . videoAssets) 0
    returnToTimeline gui project focus'
    ireturn asset'
  SAudio -> do
    enterLibrary gui (project ^. library . audioAssets) 0
    asset' <- selectAssetFromList gui (project ^. library . audioAssets) 0
    returnToTimeline gui project focus'
    ireturn asset'

selectAssetAndAppend
  :: Application t m
  => Name n
  -> Project
  -> Focus ft
  -> SMediaType mt
  -> ThroughMode TimelineMode LibraryMode (t m) n Project
selectAssetAndAppend gui project focus' mediaType =
  selectAsset gui project focus' mediaType >>= \case
    Just asset' ->
      project
        &  timeline
        %~ insert_ focus' (insertionOf asset') RightOf
        &  ireturn
    Nothing -> ireturn project
 where
  insertionOf a = case mediaType of
    SVideo -> InsertVideoPart (Clip () a)
    SAudio -> InsertAudioPart (Clip () a)

data ImportFileForm = ImportFileForm
  { selectedFile :: Maybe FilePath
  , autoSplit    :: Bool
  }

data Ok = Ok deriving (Eq, Enum)

instance DialogChoice Ok where
  toButtonLabel Ok = "OK"

importFile
  :: Application t m
  => Name n
  -> Project
  -> Focus ft
  -> ThroughMode TimelineMode ImportMode (t m) n Project
importFile gui project focus' = do
  enterImport gui
  f <- fillForm ImportFileForm {selectedFile = Nothing, autoSplit = False}
  returnToTimeline gui project focus'
  maybe (ireturn project) importAsset f
 where
  fillForm mf = do
    cmd <- nextEvent gui
    case (cmd, mf) of
      (CommandKeyMappedEvent Cancel, _) -> ireturn Nothing
      (ImportClicked, ImportFileForm { selectedFile = Just file, ..}) ->
        ireturn (Just (file, autoSplit))
      (ImportClicked, form) -> fillForm form
      (ImportFileSelected file, form) ->
        fillForm (form { selectedFile = Just file })
      (ImportAutoSplitSet s, form) -> fillForm (form { autoSplit = s })
  importAsset (filepath, True) =
    progressBar gui "Import Video" (importVideoFileAutoSplit filepath (project ^. workingDirectory))
      >>>= \case
             Nothing -> do
               iliftIO (putStrLn ("No result." :: Text))
               ireturn project
             Just assets -> handleImportResult assets
  importAsset (filepath, False) =
    progressBar gui "Import Video" (importVideoFile filepath (project ^. workingDirectory)) >>>= \case
      Nothing    -> ireturn project
      Just asset -> handleImportResult (fmap pure asset)
  handleImportResult = \case
    Left err -> do
      iliftIO (print err)
      _ <- dialog gui
                  "Import Failed!"
                  "I have no explanation at this point."
                  [Ok]
      ireturn project
    Right assets -> do
      iliftIO (print assets)
      project & library . videoAssets %~ (<> assets) & ireturn

prettyFocusedAt :: FocusedAt a -> Text
prettyFocusedAt = \case
  FocusedSequence{}  -> "sequence"
  FocusedParallel{}  -> "parallel"
  FocusedVideoPart{} -> "video track"
  FocusedAudioPart{} -> "audio track"

append
  :: Application t m
  => Name n
  -> Project
  -> Focus ft
  -> AppendCommand
  -> t m (n .== State (t m) 'TimelineMode) Empty ()
append gui project focus' cmd =
  case (cmd, atFocus focus' (project ^. timeline)) of
    (AppendComposition, Just (FocusedSequence _)) ->
      selectAsset gui project focus' SVideo >>= \case
        Just asset' ->
          project & timeline %~
          insert_
            focus'
            (InsertParallel (Parallel () [Clip () asset'] []))
            RightOf &
          timelineMode gui focus'
        Nothing -> continue
    (AppendClip, Just (FocusedVideoPart _)) ->
      selectAssetAndAppend gui project focus' SVideo >>>=
      timelineMode gui focus'
    (AppendClip, Just (FocusedAudioPart _)) ->
      selectAssetAndAppend gui project focus' SAudio >>>=
      timelineMode gui focus'
    (AppendGap, Just _) ->
      prompt
        gui
        "Insert Gap"
        "Please specify a gap duration in seconds."
        "Insert Gap"
        (NumberPrompt (0.1, 10e10)) >>>= \case
        Just seconds ->
          project
            & timeline %~ insert_ focus' (InsertVideoPart (Gap () (durationFromSeconds seconds))) RightOf
            & timelineMode gui focus'
        Nothing -> continue
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
    continue = timelineMode gui focus' project

data Confirmation
  = Yes
  | No
  deriving (Show, Eq, Enum)

instance DialogChoice Confirmation where
  toButtonLabel = \case
    Yes -> "Yes"
    No -> "No"

timelineMode
  :: Application t m
  => Name n
  -> Focus ft
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
    CommandKeyMappedEvent (AppendCommand cmd) -> append gui project focus' cmd
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
    CommandKeyMappedEvent Import ->
      importFile gui project focus' >>>= timelineMode gui focus'
    CommandKeyMappedEvent Render ->
      case Render.flattenTimeline (project ^. timeline) of
        Just flat -> do
          outDir <- iliftIO getUserDocumentsDirectory
          chooseFile gui Save "Render To File" outDir >>>= \case
            Just outFile -> do
              _ <- progressBar gui "Rendering" (Render.renderComposition 25 outFile flat)
              continue
            Nothing -> continue
        Nothing -> beep gui >>> continue
    CommandKeyMappedEvent Cancel -> continue
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

fastcut
  :: Application t m => UserInterface (t m) => Project -> t m Empty Empty ()
fastcut project = do
  start #gui keymaps project initialFocus
  timelineMode #gui initialFocus project
  where initialFocus = SequenceFocus 0 Nothing
