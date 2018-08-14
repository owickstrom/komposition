module FastCut.Progress where

import           FastCut.Prelude

newtype ProgressUpdate = ProgressUpdate Double
  deriving (Show, Eq)
