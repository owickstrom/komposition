{-# LANGUAGE LambdaCase #-}
module FastCut.Focus where

import           FastCut.Sequence

data Focus
  = InSequenceFocus Int Focus
  | InCompositionFocus ClipType Int
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
applyFocus = \case
  Sequence _ sub -> \case
    InSequenceFocus idx subFocus ->
      Sequence TransitivelyFocused (zipWith (applyAtSubSequence idx subFocus) sub [0..])
    InCompositionFocus {} -> Sequence Blurred (map blurSequence sub)
    Here -> Sequence Focused (map blurSequence sub)
  Composition _ videoClips audioClips -> \case
    InSequenceFocus {} ->
      Composition Blurred (map blurClip videoClips) (map blurClip audioClips)
    InCompositionFocus focusClipType idx ->
      let applyAtClips clips = zipWith (applyAtClip idx) clips [0..]
          (videoClips', audioClips') =
            case focusClipType of
              Video -> (applyAtClips videoClips, map blurClip audioClips)
              Audio -> (map blurClip videoClips, applyAtClips audioClips)
      in Composition TransitivelyFocused videoClips' audioClips'
    Here ->
      Composition Focused (map blurClip videoClips) (map blurClip audioClips)
  where
    applyAtSubSequence idx subFocus subSequence subIdx
      | subIdx == idx = applyFocus subSequence subFocus
      | otherwise = blurSequence subSequence
    applyAtClip idx clip clipIdx
      | clipIdx == idx = setClipAnnotation Focused clip
      | otherwise = blurClip clip

data FocusEvent = FocusUp | FocusDown | FocusLeft | FocusRight
  deriving (Eq, Show)

modifyFocus :: Sequence a -> Focus -> FocusEvent -> Maybe Focus
modifyFocus = \case
  Sequence _ sub -> \case
    InSequenceFocus idx Here -> \case
      FocusLeft | idx > 0 -> pure (InSequenceFocus (pred idx) Here)
      FocusRight | idx < (length sub - 1) -> pure (InSequenceFocus (succ idx) Here)
      _ -> Nothing
    InSequenceFocus idx subFocus -> \case
      e -> do
        InSequenceFocus idx <$> modifyFocus (sub !! fromIntegral idx) subFocus e
    InCompositionFocus {} -> const Nothing
    Here -> const (pure Here)
  Composition _ _videoClips _audioClips -> \case
    InSequenceFocus {} -> const Nothing
    InCompositionFocus _focusClipType idx ->
      const Nothing
    Here -> const (pure Here)
