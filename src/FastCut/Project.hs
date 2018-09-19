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
import           Data.Text             (Text)

import           FastCut.Composition
import           FastCut.Library
import           FastCut.VideoSettings

data Project = Project
  { _projectName        :: Text
  , _timeline           :: Timeline ()
  , _library            :: Library
  , _workingDirectory   :: FilePath
  , _videoSettings      :: VideoSettings
  , _proxyVideoSettings :: VideoSettings
  } deriving (Eq, Show, Generic)

makeLenses ''Project
