module FastCut.Progress where

import           FastCut.Prelude

data ProgressUpdate = ProgressUpdate Double Text
  deriving (Show, Eq)
