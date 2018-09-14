{-# OPTIONS_GHC -fno-warn-unticked-promoted-constructors #-}

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
module FastCut.Application where

import           FastCut.Application.Base

import           Data.Row.Records

import           FastCut.Focus
import           FastCut.Project

import           FastCut.Application.KeyMaps
import           FastCut.Application.TimelineMode

fastcut
  :: Application t m => UserInterface (t m) => Project -> t m Empty Empty ()
fastcut project' = do
  start #gui (fmap CommandKeyMappedEvent . keymaps) model
  timelineMode #gui model
  where
    initialFocus = SequenceFocus 0 Nothing
    model = TimelineModel project' initialFocus (ZoomLevel 1)
