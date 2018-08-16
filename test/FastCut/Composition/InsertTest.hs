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
    let focus = SequenceFocus 1 Nothing
        before' =
          Timeline
            ()
            (Sequence () (pure parallel1) :| [Sequence () (pure parallel2)])
        after' =
          Timeline
            ()
            (Sequence () (pure parallel1) :|
             [Sequence () (pure parallel2), Sequence () (pure parallel1)])
    insert focus (InsertSequence (Sequence () (pure parallel1))) RightOf before' `shouldBe`
      Just after'
  it "appends a video clip after the focused one" $ do
    let focus =
          SequenceFocus 0 (Just (ParallelFocus 0 (Just (ClipFocus Video 0))))
        before' =
          Timeline
            ()
            (pure (Sequence () (pure (Parallel () [video4s, video10s] []))))
        after' =
          Timeline
            ()
            (pure
               (Sequence () (pure (Parallel () [video4s, gap3s, video10s] []))))
    insert focus (InsertVideoPart gap3s) RightOf before' `shouldBe` Just after'
  it "insert a video clip into an empty parallel" $ do
    let focus =
          SequenceFocus 0 (Just (ParallelFocus 0 Nothing))
        before' =
          Timeline
            ()
            (pure (Sequence () (pure (Parallel () [] []))))
        after' =
          Timeline
            ()
            (pure
               (Sequence () (pure (Parallel () [video4s] []))))
    insert focus (InsertVideoPart video4s) LeftMost before' `shouldBe` Just after'

{-# ANN module ("HLint: ignore Use camelCase" :: Prelude.String) #-}
