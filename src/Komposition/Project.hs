{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DuplicateRecordFields      #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE OverloadedLabels           #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TemplateHaskell            #-}
module Komposition.Project where

import           Komposition.Prelude

import           Control.Lens
import           Data.Text                          (Text)

import           Komposition.Composition
import           Komposition.Library
import           Komposition.Project.UndoableAction
import           Komposition.UndoRedo
import           Komposition.VideoSettings

data Project timeline = Project
  { _projectName   :: !Text
  , _timeline      :: !timeline
  , _library       :: !Library
  , _videoSettings :: !AllVideoSettings
  }
  deriving (Eq, Show, Generic)

makeLenses ''Project

newtype ProjectPath = ProjectPath { _unProjectPath :: FilePath }
   deriving (Eq, Show, Generic, Hashable)

makeLenses ''ProjectPath

data ExistingProject timeline = ExistingProject
  { _projectPath :: !ProjectPath
  , _project     :: !(Project timeline)
  } deriving (Eq, Show)

makeLenses ''ExistingProject

type WithHistory p = p (History UndoableAction (Timeline ()))

type WithoutHistory p = p (Timeline ())


initializeHistory :: WithoutHistory ExistingProject -> WithHistory ExistingProject
initializeHistory = project . timeline %~ init

dropHistory :: WithHistory ExistingProject -> WithoutHistory ExistingProject
dropHistory = project . timeline %~ view current
