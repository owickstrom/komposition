{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

module Komposition.Composition.FocusedTest where

import           Komposition.Prelude
import qualified Prelude

import           Test.Tasty.Hspec

-- import           Komposition.Composition
-- import           Komposition.Composition.Focused
-- import           Komposition.Focus

spec_applyFocus = do
  it "adds valid foci to every part of a sequence" $
    -- TODO: should be written as a property-based test using generators
    True == True

{-# ANN module ("HLint: ignore Use camelCase" :: Prelude.String) #-}
