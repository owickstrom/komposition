{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

module FastCut.Composition.InsertTest where

import           FastCut.Prelude
import qualified Prelude

import           Test.Tasty.Hspec

import           FastCut.Composition
import           FastCut.Composition.Insert
import           FastCut.Focus
import           FastCut.MediaType

import           FastCut.TestLibrary

spec_insertRightOf = do
  it "appends a sequence after the focused one" $ do
    let focus = SequenceFocus 0 Nothing
        before' = Timeline (1 :: Int) [Sequence 2 [], Sequence 4 []]
        after' = Timeline 1 [Sequence 2 [], Sequence 3 [], Sequence 4 []]
    insert focus (InsertSequence (Sequence 3 [])) RightOf before' `shouldBe`
      Just after'
  it "appends a video clip after the focused one" $ do
    let focus =
          SequenceFocus 0 (Just (ParallelFocus 0 (Just (ClipFocus Video 0))))
        before' = Timeline () [Sequence () [Parallel () [video4s, video10s] []]]
        after' =
          Timeline () [Sequence () [Parallel () [video4s, gap3s, video10s] []]]
    insert focus (InsertVideoPart gap3s) RightOf before' `shouldBe` Just after'

{-# ANN module ("HLint: ignore Use camelCase" :: Prelude.String) #-}
