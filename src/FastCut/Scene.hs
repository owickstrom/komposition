{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE OverloadedLabels      #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE StandaloneDeriving    #-}
module FastCut.Scene where

import           Data.Semigroup
import           Data.Text        (Text)
import qualified Data.Text        as Text
import           Data.Time.Clock  (NominalDiffTime)

import           FastCut.Focus
import           FastCut.Sequence

data Scene = Scene { sceneName :: Text, topSequence :: Sequence () }
  deriving (Eq, Show)
