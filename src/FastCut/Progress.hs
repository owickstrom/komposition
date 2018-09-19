{-# LANGUAGE TemplateHaskell #-}
module FastCut.Progress where

import           FastCut.Prelude

import           Control.Lens
import           Pipes           (Producer, (>->))
import qualified Pipes.Prelude   as Pipes hiding (show)

data ProgressUpdate = ProgressUpdate
  { _progressMessage  :: Text
  , _progressFraction :: Double
  } deriving (Show, Eq)

makeLenses ''ProgressUpdate

divideProgress ::
     Monad m
  => Producer ProgressUpdate m a
  -> (a -> Producer ProgressUpdate m b)
  -> Producer ProgressUpdate m b
divideProgress m1 m2 = do
  a <- m1 >-> Pipes.map (over progressFraction (/ 2))
  m2 a >-> Pipes.map (over progressFraction ((+ 0.5) . (/ 2)))
