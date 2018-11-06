{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE ExplicitForAll        #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLabels      #-}
{-# LANGUAGE OverloadedLists       #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE RebindableSyntax      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
module Komposition.Application where

import           Komposition.Application.Base

import           Control.Effect.Carrier                    (Carrier)
import           Data.Row.Records

import           Komposition.Application.KeyMaps
import           Komposition.Application.WelcomeScreenMode

komposition
  :: ( Application t m sig
    , WelcomeScreenModeEffects sig
    , Carrier sig m
    )
  => t m Empty Empty ()
komposition = do
  start #gui (fmap CommandKeyMappedEvent . keymaps)
  welcomeScreenMode #gui
