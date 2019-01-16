{-# LANGUAGE TemplateHaskell #-}
module Komposition.Progress where

import           Komposition.Prelude

import           Control.Lens
import           Pipes               (Producer, (>->))
import qualified Pipes.Prelude       as Pipes hiding (show)

data ProgressUpdate = ProgressUpdate
  { _progressMessage  :: Text
  , _progressFraction :: Double
  } deriving (Show, Eq)

toPercent :: ProgressUpdate -> Double
toPercent (ProgressUpdate _ f) = f * 100

makeLenses ''ProgressUpdate

divideProgress2 ::
     Monad m
  => Producer ProgressUpdate m a
  -> (a -> Producer ProgressUpdate m b)
  -> Producer ProgressUpdate m b
divideProgress2 m1 m2 = do
  a <- m1 >-> Pipes.map (over progressFraction (/ 2))
  m2 a >-> Pipes.map (over progressFraction ((+ 0.5) . (/ 2)))

divideProgress3 ::
     Monad m
  => Producer ProgressUpdate m a
  -> (a -> Producer ProgressUpdate m b)
  -> (b -> Producer ProgressUpdate m c)
  -> Producer ProgressUpdate m c
divideProgress3 m1 m2 m3 = do
  a <- m1 >-> Pipes.map (over progressFraction (/ 3))
  b <- m2 a >-> Pipes.map (over progressFraction ((+ (1 / 3)) . (/ 3)))
  m3 b >-> Pipes.map (over progressFraction ((+ (2 / 3)) . (/ 3)))

divideProgress4 ::
     Monad m
  => Producer ProgressUpdate m a
  -> (a -> Producer ProgressUpdate m b)
  -> (b -> Producer ProgressUpdate m c)
  -> (c -> Producer ProgressUpdate m d)
  -> Producer ProgressUpdate m d
divideProgress4 m1 m2 m3 m4 = do
  a <- m1 >-> Pipes.map (over progressFraction (/ 4))
  b <- m2 a >-> Pipes.map (over progressFraction ((+ (1 / 4)) . (/ 4)))
  c <- m3 b >-> Pipes.map (over progressFraction ((+ (2 / 4)) . (/ 4)))
  m4 c >-> Pipes.map (over progressFraction ((+ (3 / 4)) . (/ 4)))
