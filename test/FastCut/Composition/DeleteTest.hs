{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

module FastCut.Composition.DeleteTest where

import           FastCut.Prelude
import qualified Prelude

import           Test.Tasty.Hspec

import           FastCut.Composition
import           FastCut.Composition.Delete
import           FastCut.Focus
import           FastCut.MediaType

import           FastCut.TestLibrary

spec_delete =
  it "deletes only audio part and retains valid focus" $ do
    let focus = SequenceFocus 0 (Just (ParallelFocus 0 (Just (ClipFocus Audio 0))))
        before' =
          Timeline
            (pure (Sequence () (pure (Parallel () [] [audio1s]))))
        focus' = SequenceFocus 0 (Just (ParallelFocus 0 Nothing))
        after' =
          Timeline
            (pure (Sequence () (pure (Parallel () [] []))))
    delete_ focus before' `shouldBe` Right (after', focus')

{-# ANN module ("HLint: ignore Use camelCase" :: Prelude.String) #-}
