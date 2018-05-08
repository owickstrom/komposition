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

import           Data.Kind
import           Motor.FSM

import           FastCut.Focus
import           FastCut.Project
import           FastCut.Sequence

data UserInterfaceState
  = TimelineMode
  | LibraryMode

data TimelineEvent
  = FocusEvent FocusEvent
  | OpenLibrary
  | Exit

data LibraryEvent
  = LibraryEscape
  | LibraryUp
  | LibraryDown

type family Event (t :: UserInterfaceState) where
  Event TimelineMode = TimelineEvent
  Event LibraryMode = LibraryEvent

class MonadFSM m =>
      UserInterface m where
  type State m :: UserInterfaceState -> Type
  start ::
       Name n
    -> Project
    -> Focus
    -> Actions m '[ n !+ State m TimelineMode] r ()
  updateTimeline
    :: Name n
    -> Project
    -> Focus
    -> Actions m '[ n := State m TimelineMode !--> State m TimelineMode] r ()
  enterLibrary
    :: Name n
    -> Library
    -> MediaType
    -> Int
    -> Actions m '[ n := State m TimelineMode !--> State m LibraryMode] r ()
  exitLibrary ::
       Name n
    -> Project
    -> Focus
    -> Actions m '[ n := State m LibraryMode !--> State m TimelineMode] r ()
  nextEvent ::
       Name n
    -> Actions m '[ n := Remain (State m t)] r (Event t)
  exit :: Name n -> Actions m '[ n !- State m s] r ()
