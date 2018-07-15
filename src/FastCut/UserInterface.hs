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
-- |

module FastCut.UserInterface where

import           FastCut.Prelude hiding (State)

import           Motor.FSM

import           FastCut.Focus
import           FastCut.KeyMap
import           FastCut.Library
import           FastCut.Project

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

data AppendCommand
  = AppendClip
  | AppendGap
  | AppendComposition
  deriving (Show, Eq)

data Command mode where
  Cancel :: Command mode

  FocusCommand :: FocusCommand -> Command TimelineMode
  AppendCommand :: AppendCommand -> Command TimelineMode
  Import :: Command TimelineMode
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
  exit :: Name n -> Actions m '[ n !- State m s] r ()
