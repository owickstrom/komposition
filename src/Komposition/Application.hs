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
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
module Komposition.Application where

import           Komposition.Application.Base

import           Control.Effect.Carrier                    (Carrier)
import           Data.Row.Records                          (Empty)
import           Data.String                               (fromString)

import           Komposition.Application.WelcomeScreenMode
import           Komposition.Project.Store

komposition
  :: ( Application t m sig
    , WelcomeScreenModeEffects sig
    , Carrier sig m
    )
  => t m Empty Empty ()
komposition = welcomeScreenMode

kompositionWithProject
  :: ( Application t m sig
    , WelcomeScreenModeEffects sig
    , Carrier sig m
    )
  => FilePath
  -> t m Empty Empty ()
kompositionWithProject p =
  ilift (openExistingProject p) >>= \case
    Left err -> do
      ilift (logLnText Error ("Opening existing project failed: " <> show err))
      ireturn ()
    Right existingProject' -> openTimelineWindowWithProject existingProject'
