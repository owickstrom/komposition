{-# LANGUAGE ConstraintKinds    #-}
{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE GADTs              #-}
{-# LANGUAGE KindSignatures     #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RankNTypes         #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell    #-}
{-# LANGUAGE TypeFamilies       #-}
{-# LANGUAGE TypeInType         #-}
{-# LANGUAGE TypeOperators      #-}

module Komposition.UserInterface where

import           Komposition.Prelude            hiding (State)

import           Control.Lens
import           Data.Row.Records
import           Data.Time.Clock
import           Motor.FSM                      hiding (Delete)
import           Pipes
import           Pipes.Safe                     (SafeT)

import           Komposition.Composition
import qualified Komposition.Composition.Paste  as Paste

import qualified Komposition.Composition.Insert as Insert
import           Komposition.Duration
import           Komposition.Focus
import           Komposition.History
import           Komposition.KeyMap
import           Komposition.Library
import           Komposition.MediaType
import           Komposition.Progress
import           Komposition.Project
import           Komposition.VideoSettings
import           Komposition.VideoSpeed

data Mode
  = WelcomeScreenMode
  | NewProjectMode
  | TimelineMode
  | LibraryMode
  | ImportMode

data SMode m where
  SWelcomeScreenMode :: SMode WelcomeScreenMode
  SNewProjectMode :: SMode NewProjectMode
  STimelineMode :: SMode TimelineMode
  SLibraryMode :: SMode LibraryMode
  SImportMode :: SMode ImportMode

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
  Cancel :: Command mode
  Help :: Command mode

  FocusCommand :: FocusCommand -> Command TimelineMode
  JumpFocus :: Focus SequenceFocusType -> Command TimelineMode
  InsertCommand :: InsertType -> Insert.InsertPosition -> Command TimelineMode
  Split :: Command TimelineMode
  Delete :: Command TimelineMode
  Copy :: Command TimelineMode
  Paste :: Paste.PastePosition -> Command TimelineMode
  Import :: Command TimelineMode
  Render :: Command TimelineMode
  Preview :: Command TimelineMode
  Undo :: Command TimelineMode
  Redo :: Command TimelineMode
  SaveProject :: Command TimelineMode
  CloseProject :: Command TimelineMode
  Exit :: Command TimelineMode

deriving instance Eq (Command mode)

deriving instance Ord (Command mode)

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
  Delete    -> "Delete"
  Copy      -> "Copy"
  Paste pos -> case pos of
    Paste.PasteLeftOf  -> "Paste Left Of"
    Paste.PasteRightOf -> "Paste Right Of"
  Import       -> "Import Assets"
  Render       -> "Render"
  Preview      -> "Preview"
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
  CommandKeyMappedEvent :: Command mode -> Event mode
  -- Welcome Screen
  CreateNewProjectClicked :: Event WelcomeScreenMode
  OpenExistingProjectClicked :: Event WelcomeScreenMode
  -- New Project
  ProjectNameChanged :: Text -> Event NewProjectMode
  FrameRateChanged :: FrameRate -> Event NewProjectMode
  ResolutionChanged :: Resolution -> Event NewProjectMode
  CreateClicked :: Event NewProjectMode
  -- Timeline
  ZoomLevelChanged :: ZoomLevel -> Event TimelineMode
  FocusedClipStartSet :: Duration -> Event TimelineMode
  FocusedClipEndSet :: Duration -> Event TimelineMode
  -- Import
  ImportFileSelected :: Maybe FilePath -> Event ImportMode
  ImportClassifySet :: Bool -> Event ImportMode
  ImportDefaultVideoSpeedChanged :: VideoSpeed -> Event ImportMode
  ImportClicked :: Event ImportMode
  -- Library
  LibraryAssetsSelected :: SMediaType mt -> [Asset mt] -> Event LibraryMode
  LibrarySelectionConfirmed :: Event LibraryMode
  WindowClosed :: Event mode

data ModeKeyMap where
  ModeKeyMap :: forall mode. Ord (Command mode) => SMode mode -> KeyMap (Command mode) -> ModeKeyMap

type KeyMaps = forall mode. SMode mode -> KeyMap (Event mode)

data FileChooserType
  = File
  | Directory

data FileChooserMode
  = Open FileChooserType
  | Save FileChooserType

newtype ZoomLevel = ZoomLevel Double

data TimelineModel = TimelineModel
  { _existingProject :: ExistingProject
  , _currentFocus    :: Focus SequenceFocusType
  , _statusMessage   :: Maybe Text
  , _zoomLevel       :: ZoomLevel
  , _clipboard       :: Maybe (SomeComposition ())
  }

makeLenses ''TimelineModel

data NewProjectModel = NewProjectModel
  { _newProjectName       :: Text
  , _newProjectFrameRate  :: FrameRate
  , _newProjectResolution :: Resolution
  }

makeLenses ''NewProjectModel

currentProject :: TimelineModel -> Project
currentProject = current . view (existingProject . projectHistory)

data ImportFileModel = ImportFileModel
  { classifyValue         :: Bool
  , classifyAvailable     :: Bool
  , setDefaultVideoSpeed  :: VideoSpeed
  , selectedFileMediaType :: Maybe MediaType
  }

data SelectAssetsModel mt = SelectAssetsModel
  { mediaType      :: SMediaType mt
  , allAssets      :: NonEmpty (Asset mt)
  , selectedAssets :: [Asset mt]
  }

data PromptMode ret where
  PromptNumber :: (Double, Double, Double) -> PromptMode Double
  PromptText :: PromptMode Text

class UserInterfaceMarkup markup where
  welcomeView :: markup (Event WelcomeScreenMode)
  newProjectView :: NewProjectModel -> markup (Event NewProjectMode)
  timelineView :: TimelineModel -> markup (Event TimelineMode)
  libraryView :: SelectAssetsModel mediaType -> markup (Event LibraryMode)
  importView :: ImportFileModel -> markup (Event ImportMode)

class UserInterfaceMarkup (WindowMarkup m) => WindowUserInterface m where
  type Window m :: Type -> Type
  type WindowMarkup m :: Type -> Type

  newWindow
    :: Name n
    -> WindowMarkup m event
    -> KeyMap event
    -> m r (Extend n (Window m event) r) ()

  patchWindow
    :: HasType n (Window m event) r
    => Modify n (Window m event) r ~ r
    => Name n
    -> WindowMarkup m event
    -> m r r ()

  setTransientFor
    :: HasType child (Window m e1) r
    => HasType parent (Window m e2) r
    => Name child
    -> Name parent
    -> m r r ()

  destroyWindow
    :: Name n
    -> Actions m '[ n !- Window m e] r ()

  withNewWindow
    :: ( r' ~ (n .== Window m event)
       )
    => Name n
    -> WindowMarkup m event
    -> KeyMap event
    -> m r' r' a
    -> m r r a

  withNewModalWindow
    :: ( HasType parent (Window m parentEvent) r
       , r' ~ (modal .== Window m event)
       )
    => Name parent
    -> Name modal
    -> WindowMarkup m event
    -> KeyMap event
    -> m r' r' a
    -> m r r a

  nextEvent
    :: HasType n (Window m e) r
    => Name n
    -> m r r e

  nextEventOrTimeout
    :: HasType n (Window m e) r
    => Name n
    -> DiffTime
    -> m r r (Maybe e)

  beep :: Name n -> m r r ()

  prompt
    :: HasType n (Window m event) r
    => Name n
    -> Text -- ^ Prompt window title.
    -> Text -- ^ Prompt message.
    -> Text -- ^ Button text for confirming the choice.
    -> PromptMode ret -- ^ Type of prompt, decides the return value type.
    -> m r r (Maybe ret)

  -- TODO: Move these to separate functions or widgets

  chooseFile
    :: HasType n (Window m e) r
    => Name n
    -> FileChooserMode
    -> Text -- ^ Dialog window title.
    -> FilePath
    -> m r r (Maybe FilePath)
  progressBar
    :: Exception e
    => HasType n (Window m event) r
    => Name n -- ^ Name of parent window
    -> Text -- ^ Progress window title
    -> Producer ProgressUpdate (SafeT IO) a -- ^ Progress updates producer
    -> m r r (Maybe (Either e a))
  previewStream
    :: HasType n (Window m event) r
    => Name n -- ^ Name of parent window
    -> Text -- ^ URI to stream
    -> Producer ProgressUpdate (SafeT IO) () -- ^ Streaming process
    -> VideoSettings
    -> m r r (Maybe ())
