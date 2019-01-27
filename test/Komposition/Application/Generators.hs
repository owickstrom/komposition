module Komposition.Application.Generators where

import           Komposition.Prelude

import           Control.Lens
import           Hedgehog                  hiding (Parallel (..))
import qualified Hedgehog.Gen              as Gen
import           Hedgehog.Range

import           Komposition.UserInterface
