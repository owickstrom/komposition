{-# OPTIONS_GHC -fno-warn-unticked-promoted-constructors #-}

{-# LANGUAGE ConstraintKinds  #-}
{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs            #-}
{-# LANGUAGE KindSignatures   #-}
{-# LANGUAGE RankNTypes       #-}
{-# LANGUAGE TypeFamilies     #-}
{-# LANGUAGE TypeInType       #-}
{-# LANGUAGE TypeOperators    #-}

module FastCut.UserInterface where

import           FastCut.Prelude hiding (State)

import           Motor.FSM
import           Data.Row.Records
import           Pipes

import           FastCut.Composition.Insert
import           FastCut.Focus
import           FastCut.KeyMap
import           FastCut.Library
import           FastCut.Project
import           FastCut.Progress

data Mode
  = TimelineMode
  | LibraryMode
  | ImportMode

data SMode m where
  STimelineMode :: SMode TimelineMode
  SLibraryMode :: SMode LibraryMode
  SImportMode :: SMode ImportMode

class ReturnsToTimeline (mode :: Mode)

instance ReturnsToTimeline LibraryMode
instance ReturnsToTimeline ImportMode

data InsertType
  = InsertClip
  | InsertGap
  | InsertComposition
  deriving (Show, Eq)

data Command mode where
  Cancel :: Command mode

  FocusCommand :: FocusCommand -> Command TimelineMode
  InsertCommand :: InsertType -> InsertPosition -> Command TimelineMode
  Delete :: Command TimelineMode
  Import :: Command TimelineMode
  Render :: Command TimelineMode
  Exit :: Command TimelineMode

  LibraryUp :: Command LibraryMode
  LibraryDown :: Command LibraryMode
  LibrarySelect :: Command LibraryMode

data Event mode where
  CommandKeyMappedEvent :: Command mode -> Event mode

  ImportFileSelected :: FilePath -> Event ImportMode
  ImportAutoSplitSet :: Bool -> Event ImportMode
  ImportClicked :: Event ImportMode

type KeyMaps = forall mode. SMode mode -> KeyMap (Event mode)

class Enum c => DialogChoice c where
  toButtonLabel :: c -> Text

data FileChooserMode
  = Open
  | Save

data PromptMode ret where
  NumberPrompt :: (Double, Double) -> PromptMode Double
  TextPrompt :: PromptMode Text

class MonadFSM m =>
      UserInterface m where
  type State m :: Mode -> Type
  start ::
       Name n
    -> KeyMaps
    -> Project
    -> Focus ft
    -> Actions m '[ n !+ State m TimelineMode] r ()
  updateTimeline
    :: Name n
    -> Project
    -> Focus ft
    -> Actions m '[ n := State m TimelineMode !--> State m TimelineMode] r ()
  returnToTimeline
    :: ReturnsToTimeline mode
    => Name n
    -> Project
    -> Focus ft
    -> Actions m '[ n := State m mode !--> State m TimelineMode] r ()
  enterLibrary
    :: Name n
    -> [Asset mt]
    -> Int
    -> Actions m '[ n := State m TimelineMode !--> State m LibraryMode] r ()
  updateLibrary
    :: Name n
    -> [Asset mt]
    -> Int
    -> Actions m '[ n := State m LibraryMode !--> State m LibraryMode] r ()
  enterImport
    :: Name n
    -> Actions m '[ n := State m TimelineMode !--> State m ImportMode] r ()
  nextEvent
    :: Name n
    -> Actions m '[ n := Remain (State m t)] r (Event t)
  beep :: Name n -> Actions m '[] r ()
  dialog
    :: DialogChoice c
    => Name n
    -> Text -- ^ Dialog window title.
    -> Text -- ^ Dialog message.
    -> [c] -- ^ Choices of the dialog, rendered as buttons.
    -> Actions m '[ n := Remain (State m t)] r (Maybe c)
  prompt
    :: Name n
    -> Text -- ^ Prompt window title.
    -> Text -- ^ Prompt message.
    -> Text -- ^ Button text for confirming the choice.
    -> PromptMode ret -- ^ Type of prompt, decides the return value type.
    -> Actions m '[ n := Remain (State m t)] r (Maybe ret)
  chooseFile
    :: Name n
    -> FileChooserMode
    -> Text -- ^ Dialog window title.
    -> FilePath
    -> Actions m '[ n := Remain (State m t)] r (Maybe FilePath)
  progressBar
    :: Name n
    -> Text -- ^ Progress window title.
    -> Producer ProgressUpdate IO a -- ^ Progress updates producer.
    -> Actions m '[ n := Remain (State m t)] r (Maybe a)
  exit :: Name n -> Actions m '[ n !- State m s] r ()

-- | Convenient type for actions that transition from one mode (of
-- type 'mode1') into another mode (of type 'mode2'), doing some user
-- interactions, and returning back to the first mode with a value of
-- type 'a'.
type ThroughMode mode1 mode2 m n a
   = forall i o state2 state1.
   ( UserInterface m
     , HasType n state1 i
     , HasType n state1 o
     , (Modify n state2 i .! n) ~ state2
     , Modify n state1 (Modify n state2 i) ~ o
     , Modify n state2 (Modify n state2 i) ~ Modify n state2 i
     , state1 ~ State m mode1
     , state2 ~ State m mode2
     )
   => m i o a
