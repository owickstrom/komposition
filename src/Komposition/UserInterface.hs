{-# OPTIONS_GHC -fno-warn-unticked-promoted-constructors #-}

{-# LANGUAGE ConstraintKinds    #-}
{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE GADTs              #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RankNTypes         #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell    #-}
{-# LANGUAGE TypeFamilies       #-}
{-# LANGUAGE TypeInType         #-}
{-# LANGUAGE TypeOperators      #-}

module Komposition.UserInterface where

import           Komposition.Prelude     hiding ( State )

import           Control.Lens

import qualified Komposition.Composition.Paste as Paste
import qualified Komposition.Composition.Insert
                                               as Insert
import           Komposition.Duration
import           Komposition.Focus
import           Komposition.KeyMap
import           Komposition.Library
import           Komposition.MediaType
import           Komposition.Project
import           Komposition.VideoSettings
import           Komposition.VideoSpeed
import           Komposition.UserInterface.WindowUserInterface

data Mode
  = WelcomeScreenMode
  | NewProjectMode
  | TimelineMode
  | LibraryMode
  | ImportMode

data SMode m where
  SWelcomeScreenMode ::SMode WelcomeScreenMode
  SNewProjectMode ::SMode NewProjectMode
  STimelineMode ::SMode TimelineMode
  SLibraryMode ::SMode LibraryMode
  SImportMode ::SMode ImportMode

modeTitle :: SMode m -> Text
modeTitle = \case
  SWelcomeScreenMode -> "Welcome Screen Mode"
  SNewProjectMode    -> "New Project Mode"
  STimelineMode      -> "Timeline Mode"
  SLibraryMode       -> "Library Mode"
  SImportMode        -> "Import Mode"

data InsertType
  = InsertComposition
  | InsertClip (Maybe MediaType)
  | InsertGap (Maybe MediaType)
  deriving (Show, Eq, Ord)

data Command (mode :: Mode) where
  Cancel ::Command mode
  Help ::Command mode

  FocusCommand ::FocusCommand -> Command TimelineMode
  JumpFocus ::Focus SequenceFocusType -> Command TimelineMode
  InsertCommand ::InsertType -> Insert.InsertPosition -> Command TimelineMode
  Split ::Command TimelineMode
  Join ::Command TimelineMode
  Delete ::Command TimelineMode
  Copy ::Command TimelineMode
  Paste ::Paste.PastePosition -> Command TimelineMode
  Import ::Command TimelineMode
  Render ::Command TimelineMode
  PlayOrStop ::Command TimelineMode
  Undo ::Command TimelineMode
  Redo ::Command TimelineMode
  SaveProject ::Command TimelineMode
  CloseProject ::Command TimelineMode
  Exit ::Command TimelineMode

deriving instance Eq (Command mode)

deriving instance Ord (Command mode)

deriving instance Show (Command mode)

commandName :: Command mode -> Text
commandName = \case
  Cancel           -> "Cancel"
  Help             -> "Show Help"
  FocusCommand cmd -> case cmd of
    FocusUp    -> "Move Focus Up"
    FocusDown  -> "Move Focus Down"
    FocusLeft  -> "Move Focus Left"
    FocusRight -> "Move Focus Right"
  JumpFocus _                             -> "Jump Focus To"
  InsertCommand insertType insertPosition -> mconcat
    [insertTypeName insertType, " (", insertPositionName insertPosition, ")"]
  Split     -> "Split"
  Join      -> "Join"
  Delete    -> "Delete"
  Copy      -> "Copy"
  Paste pos -> case pos of
    Paste.PasteLeftOf  -> "Paste Left Of"
    Paste.PasteRightOf -> "Paste Right Of"
  Import       -> "Import Assets"
  Render       -> "Render"
  PlayOrStop   -> "Play/Stop"
  Undo         -> "Undo"
  Redo         -> "Redo"
  SaveProject  -> "Save"
  CloseProject -> "Close"
  Exit         -> "Exit"
  where
    insertTypeName :: InsertType -> Text
    insertTypeName = \case
      InsertClip Nothing      -> "Insert Clip"
      InsertGap  Nothing      -> "Insert Gap"
      InsertClip (Just Video) -> "Insert Video Clip"
      InsertGap  (Just Video) -> "Insert Video Gap"
      InsertClip (Just Audio) -> "Insert Audio Clip"
      InsertGap  (Just Audio) -> "Insert Audio Gap"
      InsertComposition       -> "Insert Composition"
    insertPositionName :: Insert.InsertPosition -> Text
    insertPositionName = \case
      Insert.LeftMost  -> "Leftmost"
      Insert.LeftOf    -> "Left of"
      Insert.RightOf   -> "Right of"
      Insert.RightMost -> "Rightmost"

data Event mode where
  CommandKeyMappedEvent ::Show (Command mode) => Command mode -> Event mode
  -- Welcome Screen
  CreateNewProjectClicked ::Event WelcomeScreenMode
  OpenExistingProjectClicked ::Event WelcomeScreenMode
  -- New Project
  ProjectNameChanged ::Text -> Event NewProjectMode
  FrameRateChanged ::FrameRate -> Event NewProjectMode
  ResolutionChanged ::Resolution -> Event NewProjectMode
  CreateClicked ::Event NewProjectMode
  -- Timeline
  ZoomLevelChanged ::ZoomLevel -> Event TimelineMode
  PreviewFrameExtracted ::FilePath -> Event TimelineMode
  FocusedClipSpeedSet ::VideoSpeed -> Event TimelineMode
  FocusedClipStartSet ::Duration -> Event TimelineMode
  FocusedClipEndSet ::Duration -> Event TimelineMode
  StreamingProcessFailed ::Text -> Event TimelineMode
  PlaybackProgress ::Double -> Event TimelineMode
  PlaybackRestarting ::Event TimelineMode
  PlaybackFinished ::Event TimelineMode
  -- Import
  ImportFileSelected ::Maybe FilePath -> Event ImportMode
  ImportClassifySet ::Bool -> Event ImportMode
  ImportDefaultVideoSpeedChanged ::VideoSpeed -> Event ImportMode
  ImportClicked ::Event ImportMode
  -- Library
  LibraryAssetsSelected
    ::(Show (SMediaType mt), Show (Asset mt))
    => SMediaType mt
    -> [Asset mt]
    -> Event LibraryMode
  LibrarySelectionConfirmed ::Event LibraryMode
  WindowClosed ::Event mode

deriving instance Show (Event mode)

data ModeKeyMap where
  ModeKeyMap :: Ord (Command mode) => SMode mode -> KeyMap (Command mode) -> ModeKeyMap

type KeyMaps = forall mode . SMode mode -> KeyMap (Event mode)

newtype ZoomLevel = ZoomLevel Double
  deriving (Eq, Show)

data Preview
  = PlayHttpStream Text Word Duration
  | PlayFile FilePath Duration
  | PreviewFrame FilePath
  deriving (Eq, Show)

data TimelineViewModel = TimelineViewModel
  { _project          :: WithHistory Project
  , _currentFocus     :: Focus SequenceFocusType
  , _statusMessage    :: Maybe Text
  , _zoomLevel        :: ZoomLevel
  , _preview          :: Maybe Preview
  , _playbackProgress :: Maybe Double
  } deriving (Eq, Show)

makeLenses ''TimelineViewModel

data NewProjectModel = NewProjectModel
  { _newProjectName       :: Text
  , _newProjectFrameRate  :: FrameRate
  , _newProjectResolution :: Resolution
  }

makeLenses ''NewProjectModel

data ImportFileModel = ImportFileModel
  { classifyValue         :: Bool
  , classifyAvailable     :: Bool
  , setDefaultVideoSpeed  :: VideoSpeed
  , selectedFileMediaType :: Maybe MediaType
  }

data SelectAssetsModel mt where
  SelectAssetsModel
    ::( Show (Asset mt)
       , Show (SMediaType mt)
       )
    => { mediaType      :: SMediaType mt
       , allAssets      :: NonEmpty (Asset mt)
       , selectedAssets :: [Asset mt]
       }
    -> SelectAssetsModel mt

class UserInterfaceMarkup markup where
  welcomeView :: markup TopWindow (Event WelcomeScreenMode)
  newProjectView :: NewProjectModel -> markup Modal (Event NewProjectMode)
  timelineView :: TimelineViewModel -> markup TopWindow (Event TimelineMode)
  libraryView :: SelectAssetsModel mediaType -> markup Modal (Event LibraryMode)
  importView :: ImportFileModel -> markup Modal (Event ImportMode)

