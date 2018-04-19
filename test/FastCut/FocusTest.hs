{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

module FastCut.FocusTest where

import           Test.Tasty.Hspec

import           FastCut.Focus
import           FastCut.Sequence


video1 = VideoClip () (ClipMetadata "video-1" "/tmp/1.mp4" 4)
video2 = VideoClip () (ClipMetadata "video-2" "/tmp/2.mp4" 10)
audio1 = AudioClip () (ClipMetadata "audio-1" "/tmp/1.m4a" 5)
audio2 = AudioClip () (ClipMetadata "audio-2" "/tmp/2.m4a" 8)
audio3 = AudioClip () (ClipMetadata "audio-3" "/tmp/3.m4a" 5)
gap1 = VideoGap () 1
gap2 = VideoGap () 3
composition1 = Composition () [gap1, video1] [audio1]
composition2 = Composition () [gap2, video2] [audio2, audio3]

singleLevelSequence = Sequence
  ()
  [ Composition () [gap1, video1] [audio1]
  , Composition () [gap2, video2] [audio2, audio3]
  ]

twoLevelSequence =
  Sequence () [Sequence () [], Sequence () [composition1, composition2]]

spec_applyFocus = do

  it "applies focus to first-level sequence"
    $          applyFocus (Sequence () [Sequence () [Composition () [VideoGap () 1] []]])
                          (InSequenceFocus 0 Nothing)
    `shouldBe` Sequence TransitivelyFocused [Sequence Focused [Composition Blurred [VideoGap Blurred 1] []]]

  it "applies focus to second-level composition"
    $          applyFocus (Sequence () [Sequence () [Composition () [VideoGap () 1] []]])
                          (InSequenceFocus 0 (Just (InSequenceFocus 0 Nothing)))
    `shouldBe` Sequence TransitivelyFocused [Sequence TransitivelyFocused [Composition Focused [VideoGap Blurred 1] []]]

spec_modifyFocus = do

  it "moves the focus left within a single-level sequence" $ do
    let before' = InSequenceFocus 1 Nothing
        after'  = InSequenceFocus 0 Nothing
    modifyFocus singleLevelSequence FocusLeft before' `shouldBe` pure after'

  it "maintains sequence focus when left move is out of bounds" $ do
    let before' = InSequenceFocus 0 Nothing
    modifyFocus singleLevelSequence FocusLeft before'
      `shouldBe` Left (OutOfBounds singleLevelSequence FocusLeft before')

  it "maintains sequence focus when right move is out of bounds" $ do
    let before' = InSequenceFocus 1 Nothing
    modifyFocus singleLevelSequence FocusRight before'
      `shouldBe` Left (OutOfBounds singleLevelSequence FocusRight before')

  it "moves the focus left within innermost sequence of multi-level sequence"
    $ do
        let before' = InSequenceFocus 1 (Just (InSequenceFocus 1 Nothing))
            after'  = InSequenceFocus 1 (Just (InSequenceFocus 0 Nothing))
        modifyFocus twoLevelSequence FocusLeft before' `shouldBe` pure after'

  it "maintains composition focus when left move is out of bounds" $ do
    let before' = InSequenceFocus 0 (Just (InCompositionFocus Video 0))
    modifyFocus singleLevelSequence FocusLeft before' `shouldBe` Left
      (OutOfBounds composition1 FocusLeft (InCompositionFocus Video 0))

  it "maintains composition focus when right move is out of bounds" $ do
    let before' = InSequenceFocus 0 (Just (InCompositionFocus Video 1))
    modifyFocus singleLevelSequence FocusRight before' `shouldBe` Left
      (OutOfBounds composition1 FocusRight (InCompositionFocus Video 1))

  it "moves the focus up from innermost sequence of multi-level sequence" $ do
    let before' = InSequenceFocus 1 (Just (InSequenceFocus 1 Nothing))
        after'  = InSequenceFocus 1 Nothing
    modifyFocus twoLevelSequence FocusUp before' `shouldBe` pure after'

  it "moves the focus up from composition in multi-level sequence" $ do
    let before' = InSequenceFocus
          1
          (Just (InSequenceFocus 1 (Just (InCompositionFocus Video 1))))
        after' = InSequenceFocus 1 (Just (InSequenceFocus 1 Nothing))
    modifyFocus twoLevelSequence FocusUp before' `shouldBe` pure after'

  it "maintains top sequence focus when trying to move up" $ do
    let before' = InSequenceFocus 0 Nothing
    modifyFocus twoLevelSequence FocusUp before' `shouldBe` Left CannotMoveUp

  it "moves the focus down into a composition in single-level sequence" $ do
    let before' = InSequenceFocus 0 Nothing
        after'  = InSequenceFocus 0 (Just (InCompositionFocus Video 0))
    modifyFocus singleLevelSequence FocusDown before' `shouldBe` pure after'

{-# ANN module ("HLint: ignore Use camelCase" :: String) #-}
