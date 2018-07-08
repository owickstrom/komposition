{-# OPTIONS_GHC -fno-warn-unticked-promoted-constructors #-}

{-# LANGUAGE ConstraintKinds  #-}
{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE KindSignatures   #-}
{-# LANGUAGE RankNTypes       #-}
{-# LANGUAGE TypeFamilies     #-}
{-# LANGUAGE TypeInType       #-}
{-# LANGUAGE TypeOperators    #-}
-- |

module FastCut.UserInterface where

import           FastCut.Prelude  hiding (State)

import           Motor.FSM

import           FastCut.Focus
import           FastCut.KeyMap
import           FastCut.Project
import           FastCut.Sequence

data UserInterfaceState
  = TimelineMode
  | LibraryMode

newtype Event = KeyPress KeyCombo

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
  nextEvent ::
       Name n
    -> Actions m '[ n := Remain (State m t)] r Event
  beep :: Name n -> Actions m '[] r ()
  exit :: Name n -> Actions m '[ n !- State m s] r ()
