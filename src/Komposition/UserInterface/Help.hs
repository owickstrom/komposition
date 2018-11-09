{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLabels      #-}
{-# LANGUAGE OverloadedLists       #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE TypeOperators         #-}
module Komposition.UserInterface.Help where

import           Komposition.Prelude

import           Data.Row.Records
import           Komposition.KeyMap
import           Komposition.UserInterface
import           Motor.FSM

data HelpEvent
  = HelpClosed

class HelpView markup where
  helpView :: [ModeKeyMap] -> markup HelpEvent

help
  :: ( IxMonad m
     , HelpView (WindowMarkup m)
     , WindowUserInterface m
     , HasType parent (Window m parentEvent) r
     )
  => Name parent
  -> [ModeKeyMap]
  -> m r r (Maybe c)
help parent keyMap =
  withNewModalWindow parent #help (helpView keyMap) helpKeyMap
    $    nextEvent #help
    >>>= \case
           HelpClosed -> ireturn Nothing
  where
    helpKeyMap :: KeyMap HelpEvent
    helpKeyMap = KeyMap
      [([KeyChar 'q'], Mapping HelpClosed), ([KeyEscape], Mapping HelpClosed)]
