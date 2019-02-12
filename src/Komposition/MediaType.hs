{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE GADTs              #-}
{-# LANGUAGE KindSignatures     #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies       #-}
module Komposition.MediaType where

import           Komposition.Prelude

data MediaType = Video | Audio
  deriving (Eq, Show, Ord, Generic)

data SMediaType (mt :: MediaType) where
  SVideo :: SMediaType Video
  SAudio :: SMediaType Audio

deriving instance Eq (SMediaType mt)
deriving instance Show (SMediaType mt)
deriving instance Ord (SMediaType mt)

type family InverseMediaType (t :: MediaType) :: MediaType where
  InverseMediaType Video = Audio
  InverseMediaType Audio = Video
