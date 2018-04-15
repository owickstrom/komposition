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
  { name     :: Text
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
  = Sequenced [Sequence]
  | Composed [Clip Video] [Clip Audio]
  deriving (Eq, Show)

single :: Clip a -> Sequence
single c = case c of
  VideoClip{} -> Composed [c] []
  VideoGap{}  -> Composed [c] []
  AudioClip{} -> Composed [] [c]
  AudioGap{}  -> Composed [] [c]

instance Semigroup Sequence where
  Sequenced a <> Sequenced b = Sequenced (a <> b)
  Sequenced a <> b = Sequenced (a <> [b])
  a <> Sequenced b = Sequenced (a : b)
  a <> b = Sequenced [a, b]

instance Monoid Sequence where
  mempty = Sequenced []

data Scene = Scene { name :: Text, sequence :: Sequence }
  deriving (Eq, Show)
