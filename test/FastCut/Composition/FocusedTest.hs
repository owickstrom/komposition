{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

module FastCut.Composition.FocusedTest where

import           FastCut.Prelude
import qualified Prelude

import           Test.Tasty.Hspec

-- import           FastCut.Composition
-- import           FastCut.Composition.Focused
-- import           FastCut.Focus

spec_applyFocus = do
  it "adds valid foci to every part of a sequence" $
    -- TODO: should be written as a property-based test using generators
    True == True

{-# ANN module ("HLint: ignore Use camelCase" :: Prelude.String) #-}
