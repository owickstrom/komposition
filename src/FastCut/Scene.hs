{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE OverloadedLabels      #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE StandaloneDeriving    #-}
module FastCut.Scene where

import           Data.Semigroup
import           Data.Text       (Text)
import qualified Data.Text       as Text
import           Data.Time.Clock (NominalDiffTime)

type Duration = NominalDiffTime

data ClipMetadata = ClipMetadata
  { clipName :: Text
  , path     :: FilePath
  , duration :: Duration
  } deriving (Eq, Show)

data ClipType = Video | Audio

data Clip (a :: ClipType) where
  VideoClip :: ClipMetadata -> Clip Video
  AudioClip :: ClipMetadata -> Clip Audio
  VideoGap :: Duration -> Clip Video
  AudioGap :: Duration -> Clip Audio

deriving instance Eq (Clip a)
deriving instance Show (Clip a)

data Sequence
  = Sequence [Sequence]
  | Composition [Clip Video] [Clip Audio]
  deriving (Eq, Show)

single :: Clip a -> Sequence
single c = case c of
  VideoClip{} -> Composition [c] []
  VideoGap{}  -> Composition [c] []
  AudioClip{} -> Composition [] [c]
  AudioGap{}  -> Composition [] [c]

instance Semigroup Sequence where
  Sequence a <> Sequence b = Sequence (a <> b)
  Sequence a <> b = Sequence (a <> [b])
  a <> Sequence b = Sequence (a : b)
  a <> b = Sequence [a, b]

instance Monoid Sequence where
  mempty = Sequence []
  mappend = (<>)

data Scene = Scene { sceneName :: Text, topSequence :: Sequence }
  deriving (Eq, Show)
