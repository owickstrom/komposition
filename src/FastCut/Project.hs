{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE OverloadedLabels      #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE TemplateHaskell       #-}
module FastCut.Project where

import           FastCut.Prelude

import           Control.Lens
import           Data.Text           (Text)

import           FastCut.Composition
import           FastCut.Library

data Project = Project
  { _projectName      :: Text
  , _timeline         :: Timeline ()
  , _library          :: Library
  , _workingDirectory :: FilePath
  } deriving (Eq, Show, Generic)

makeLenses ''Project
