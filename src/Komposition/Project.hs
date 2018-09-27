{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE OverloadedLabels      #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE TemplateHaskell       #-}
module Komposition.Project where

import           Komposition.Prelude

import           Control.Lens
import           Data.Text             (Text)

import           Komposition.Composition
import           Komposition.Library
import           Komposition.VideoSettings

data Project = Project
  { _projectName        :: Text
  , _timeline           :: Timeline ()
  , _library            :: Library
  , _workingDirectory   :: FilePath
  , _videoSettings      :: VideoSettings
  , _proxyVideoSettings :: VideoSettings
  } deriving (Eq, Show, Generic)

makeLenses ''Project
