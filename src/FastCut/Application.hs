{-# LANGUAGE ConstraintKinds  #-}
{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE ExplicitForAll   #-}
{-# LANGUAGE LambdaCase       #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE PolyKinds        #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE TypeFamilies     #-}
{-# LANGUAGE TypeOperators    #-}
-- |
module FastCut.Application where

import           Prelude                  hiding ((>>))

import           Control.Lens
import           Data.Row.Records
import           GHC.OverloadedLabels
import           Motor.FSM

import           Control.Monad.Indexed.IO
import           FastCut.Focus
import           FastCut.Project
import           FastCut.Sequence
import           FastCut.UserInterface

libraryMode ::
    (UserInterface m, IxMonadIO m)
  => Name n
  -> Project
  -> Focus
  -> m (n .== State m 'LibraryMode) Empty ()
libraryMode gui project focus' =
  nextEvent gui >>>= \case
    LibraryEscape -> do
      exitLibrary gui project focus'
      timelineMode gui project focus'
    _ ->
      libraryMode gui project focus'
  where
    (>>) = (>>>)

timelineMode ::
     (UserInterface m, IxMonadIO m)
  => Name n
  -> Project
  -> Focus
  -> m (n .== State m 'TimelineMode) Empty ()
timelineMode gui project focus' = do
  updateTimeline gui project focus'
  nextEvent gui >>>= \case
    FocusEvent e ->
      case modifyFocus (project ^. topSequence) e focus' of
        Left _err      -> timelineMode gui project focus'
        Right newFocus -> timelineMode gui project newFocus
    OpenLibrary -> do
      enterLibrary gui (project ^. library) Video 0
      libraryMode gui project focus'
    Exit -> exit gui
  where
    (>>) = (>>>)

fastcut :: (IxMonadIO m) => UserInterface m => Project -> m Empty Empty ()
fastcut project =
  start #gui project initialFocus
  >>> timelineMode #gui project initialFocus
  where
    initialFocus = InSequenceFocus 0 Nothing
