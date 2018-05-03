{-# OPTIONS_GHC -fno-warn-unticked-promoted-constructors #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE OverloadedLabels      #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE TemplateHaskell       #-}
module FastCut.Project where

import           Control.Lens
import           Data.Text        (Text)

import           FastCut.Focus
import           FastCut.Sequence

data Library = Library
  { _videoClips :: [Clip () Video]
  , _audioClips :: [Clip () Audio]
  } deriving (Eq, Show)

makeLenses ''Library

instance Semigroup Library where
  l1 <> l2 =
    Library
    { _videoClips = _videoClips l1 <> _videoClips l2
    , _audioClips = _audioClips l1 <> _audioClips l2
    }

instance Monoid Library where
  mempty = Library mempty mempty

data Project = Project
  { _projectName :: Text
  , _topSequence :: Sequence ()
  , _library     :: Library
  } deriving (Eq, Show)

makeLenses ''Project

appendAt :: Focus -> Sequence () -> Sequence ()
appendAt =
  withParentOf onSequence onVideoClips onAudioClips
  where
    onSequence _ = id
    onVideoClips _ = id
    onAudioClips _ = id
