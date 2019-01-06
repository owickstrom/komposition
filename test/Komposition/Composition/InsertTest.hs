{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

module Komposition.Composition.InsertTest where

import           Komposition.Prelude
import qualified Prelude

import           Test.Tasty.Hspec

import           Komposition.Composition
import           Komposition.Composition.Insert
import           Komposition.Focus
import           Komposition.MediaType

import           Komposition.TestLibrary

spec_insertRightOf = do
  it "appends a sequence after the focused one" $ do
    let focus' = SequenceFocus 1 Nothing
        before' =
          Timeline
            (Sequence () (pure parallel1) :| [Sequence () (pure parallel2)])
        after' =
          Timeline
            (Sequence () (pure parallel1) :|
             [Sequence () (pure parallel2), Sequence () (pure parallel1)])
    insert focus' (InsertSequence (Sequence () (pure parallel1))) RightOf before' `shouldBe`
      Just after'
  it "appends a video clip after the focused one" $ do
    let focus' =
          SequenceFocus 0 (Just (ParallelFocus 0 (Just (ClipFocus Video 0))))
        before' =
          Timeline
            (pure (Sequence () (pure (Parallel () [video4s, video10s] []))))
        after' =
          Timeline
            (pure
               (Sequence () (pure (Parallel () [video4s, videoGap3s, video10s] []))))
    insert focus' (InsertVideoParts [videoGap3s]) RightOf before' `shouldBe` Just after'
  it "insert a video clip into an empty parallel" $ do
    let focus' =
          SequenceFocus 0 (Just (ParallelFocus 0 Nothing))
        before' =
          Timeline
            (pure (Sequence () (pure (Parallel () [] []))))
        after' =
          Timeline
            (pure
               (Sequence () (pure (Parallel () [video4s] []))))
    insert focus' (InsertVideoParts [video4s]) LeftMost before' `shouldBe` Just after'

{-# ANN module ("HLint: ignore Use camelCase" :: Prelude.String) #-}
