{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

module FastCut.Composition.FocusedTest where

import           FastCut.Prelude
import qualified Prelude

import           Test.Tasty.Hspec

import           FastCut.Composition
import           FastCut.Composition.Focused
import           FastCut.Focus

spec_applyFocus = do
  it "applies focus to sequence" $
    applyFocus
      (Timeline () (pure (Sequence () (pure (Parallel () [Gap () 1] [])))))
      (SequenceFocus 0 Nothing) `shouldBe`
    Timeline
      TransitivelyFocused
      (pure (Sequence Focused (pure (Parallel Blurred [Gap Blurred 1] []))))
  it "applies focus to parallel" $
    applyFocus
      (Timeline () (pure (Sequence () (pure (Parallel () [Gap () 1] [])))))
      (SequenceFocus 0 (Just (ParallelFocus 0 Nothing))) `shouldBe`
    Timeline
      TransitivelyFocused
      (pure
         (Sequence
            TransitivelyFocused
            (pure (Parallel Focused [Gap Blurred 1] []))))

{-# ANN module ("HLint: ignore Use camelCase" :: Prelude.String) #-}
