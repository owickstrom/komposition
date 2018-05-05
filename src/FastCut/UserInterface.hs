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
    -> Actions m '[ n :-> State m TimelineMode !--> State m TimelineMode] r ()
  nextTimelineEvent ::
       Name n
    -> Actions m '[ n :-> Remain (State m TimelineMode)] r TimelineEvent
  enterLibrary
    :: Name n
    -> Library
    -> ClipType
    -> Int
    -> Actions m '[ n :-> State m TimelineMode !--> State m LibraryMode] r ()
  nextLibraryEvent ::
       Name n
    -> Actions m '[ n :-> Remain (State m LibraryMode)] r LibraryEvent
  exitLibrary ::
       Name n
    -> Project
    -> Focus
    -> Actions m '[ n :-> State m LibraryMode !--> State m TimelineMode] r ()
  exit :: Name n -> Actions m '[ n !- State m s] r ()
