{-# LANGUAGE LambdaCase #-}
module FastCut.Focus where

import           FastCut.Sequence

data Focus
  = InSequenceFocus Word Focus
  | InCompositionFocus ClipType Word
  | Here
  deriving (Eq, Show)

data Focused
  = Focused
  | TransitivelyFocused
  | Blurred
  deriving (Eq, Show)

blurSequence :: Sequence a -> Sequence Focused
blurSequence = \case
  Sequence _ sub -> Sequence Blurred (map blurSequence sub)
  Composition _ videoClips audioClips ->
    Composition
    Blurred
    (map blurClip videoClips)
    (map blurClip audioClips)

blurClip :: Clip a t -> Clip Focused t
blurClip = setClipAnnotation Blurred

applyFocus :: Sequence a -> Focus -> Sequence Focused
applyFocus sequence focus = case (sequence, focus) of
  (Sequence ann sub, InSequenceFocus idx subFocus) ->
    Sequence TransitivelyFocused (zipWith (applyAtSubSequence idx subFocus) sub [0..])
  (Composition ann videoClips audioClips, InCompositionFocus focusClipType idx) ->
    let applyAtClips clips = zipWith (applyAtClip idx) clips [0..]
        (videoClips', audioClips') =
          case focusClipType of
            Video -> (applyAtClips videoClips, map blurClip audioClips)
            Audio -> (map blurClip videoClips, applyAtClips audioClips)
     in Composition TransitivelyFocused videoClips' audioClips'
  where
    applyAtSubSequence idx subFocus subSequence subIdx
      | subIdx == idx = applyFocus subSequence subFocus
      | otherwise = blurSequence subSequence
    applyAtClip idx clip clipIdx
      | clipIdx == idx = setClipAnnotation Focused clip
      | otherwise = blurClip clip
