{-# OPTIONS_GHC -fno-warn-unticked-promoted-constructors #-}
{-# LANGUAGE DataKinds      #-}
{-# LANGUAGE GADTs          #-}
{-# LANGUAGE KindSignatures #-}
module FastCut.MediaType where

import           FastCut.Prelude

data MediaType = Video | Audio
  deriving (Eq, Show, Ord)

data SMediaType (mt :: MediaType) where
  SVideo :: SMediaType Video
  SAudio :: SMediaType Audio
