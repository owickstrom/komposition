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

import           FastCut.Prelude             hiding (State)

import           Motor.FSM

import           FastCut.Composition
import           FastCut.Composition.Focused
import           FastCut.Focus
import           FastCut.KeyMap
import           FastCut.Project

data UserInterfaceState
  = TimelineMode
  | LibraryMode
  | ImportMode

data Event (s :: UserInterfaceState) where
  KeyPress :: KeyCombo -> Event s
  ImportFileSelected :: FilePath -> Event ImportMode
  ImportClicked :: Event ImportMode

class MonadFSM m =>
      UserInterface m where
  type State m :: UserInterfaceState -> Type
  start ::
       Name n
    -> Project
    -> Focus ft
    -> Actions m '[ n !+ State m TimelineMode] r ()
  updateTimeline
    :: Name n
    -> Project
    -> Focus ft
    -> Actions m '[ n := State m TimelineMode !--> State m TimelineMode] r ()
  enterLibrary
    :: Name n
    -> Actions m '[ n := State m TimelineMode !--> State m LibraryMode] r ()
  updateLibrary
    :: Name n
    -> [Clip Focused mt]
    -> Actions m '[ n := State m LibraryMode !--> State m LibraryMode] r ()
  exitLibrary ::
       Name n
    -> Project
    -> Focus ft
    -> Actions m '[ n := State m LibraryMode !--> State m TimelineMode] r ()
  enterImport
    :: Name n
    -> Actions m '[ n := State m TimelineMode !--> State m ImportMode] r ()
  exitImport
    :: Name n
    -> Project
    -> Focus ft
    -> Actions m '[ n := State m ImportMode !--> State m TimelineMode] r ()
  nextEvent ::
       Name n
    -> Actions m '[ n := Remain (State m t)] r (Event t)
  beep :: Name n -> Actions m '[] r ()
  exit :: Name n -> Actions m '[ n !- State m s] r ()
