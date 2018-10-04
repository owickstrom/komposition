{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE ExplicitForAll        #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLabels      #-}
{-# LANGUAGE OverloadedLists       #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE RebindableSyntax      #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
module Komposition.Application where

import           Komposition.Application.Base

import           Data.Row.Records

import           Komposition.Focus
import           Komposition.Project
import           Komposition.History

import           Komposition.Application.KeyMaps
import           Komposition.Application.TimelineMode

komposition
  :: Application t m => UserInterface (t m) => Project -> t m Empty Empty ()
komposition project' = do
  start #gui (fmap CommandKeyMappedEvent . keymaps) model
  timelineMode #gui model
  where
    initialFocus = SequenceFocus 0 Nothing
    model = TimelineModel (initialise project') initialFocus Nothing (ZoomLevel 1)
