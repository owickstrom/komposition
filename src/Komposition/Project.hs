{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DuplicateRecordFields      #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE OverloadedLabels           #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE TemplateHaskell            #-}
module Komposition.Project where

import           Komposition.Prelude

import           Control.Lens
import           Data.Text                 (Text)

import           Komposition.Composition
import           Komposition.History
import           Komposition.Library
import           Komposition.VideoSettings

data Project = Project
  { _projectName   :: !Text
  , _timeline      :: !(Timeline ())
  , _library       :: !Library
  , _videoSettings :: !AllVideoSettings
  } deriving (Eq, Show, Generic)

makeLenses ''Project

newtype ProjectPath = ProjectPath { _unProjectPath :: FilePath }
   deriving (Eq, Show, Generic, Hashable)

makeLenses ''ProjectPath

data ExistingProject = ExistingProject
  { _projectPath    :: !ProjectPath
  , _projectHistory :: !(History Project)
  } deriving (Eq, Show)

makeLenses ''ExistingProject
