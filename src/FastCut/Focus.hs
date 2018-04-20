{-# LANGUAGE LambdaCase #-}
module FastCut.Focus
where

import           Control.Monad.Except (throwError)
import           FastCut.Sequence

data Focus
  = InSequenceFocus Int (Maybe Focus)
  | InCompositionFocus ClipType Int
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
    Composition Blurred (map blurClip videoClips) (map blurClip audioClips)

blurClip :: Clip a t -> Clip Focused t
blurClip = setClipAnnotation Blurred

applyFocus :: Sequence a -> Focus -> Sequence Focused
applyFocus s f = go s (Just f)
 where

  go (Sequence _ sub) (Just (InSequenceFocus idx subFocus)) = Sequence
    TransitivelyFocused
    (zipWith (applyAtSubSequence idx subFocus) sub [0 ..])
  go (Sequence _ sub) _ = Sequence Focused (map blurSequence sub)
  go (Composition _ videoClips audioClips) (Just (InCompositionFocus focusClipType idx))
    = let focusInClips clips = zipWith (focusClipAt idx) clips [0 ..]
          (videoClips', audioClips') = case focusClipType of
            Video -> (focusInClips videoClips, map blurClip audioClips)
            Audio -> (map blurClip videoClips, focusInClips audioClips)
      in  Composition TransitivelyFocused videoClips' audioClips'
  go (Composition _ videoClips audioClips) _ =
    Composition Focused (map blurClip videoClips) (map blurClip audioClips)
  -- Apply focus at the sub-sequence specified by 'idx'.
  applyAtSubSequence idx subFocus subSequence subIdx
    | subIdx == idx = go subSequence subFocus
    | otherwise     = blurSequence subSequence
  -- Apply focus at the clip specified by 'idx'.
  focusClipAt idx clip clipIdx
    | clipIdx == idx = setClipAnnotation Focused clip
    | otherwise      = blurClip clip

data FocusEvent = FocusUp | FocusDown | FocusLeft | FocusRight
  deriving (Eq, Show)

data FocusError a
  = OutOfBounds (Sequence a) FocusEvent Focus
  | CannotMoveUp
  | UnhandledFocusModification (Sequence a) FocusEvent Focus
  deriving (Eq, Show)

modifyFocus :: Sequence a -> FocusEvent -> Focus -> Either (FocusError a) Focus
modifyFocus s e f = case (s, e, f) of

  -- * Up
  -- We can move up from audio to video within a composition.
  (Composition{}, FocusUp, InCompositionFocus Audio _) ->
    pure (InCompositionFocus Video 0)
  --  In these cases we've hit a focus "leaf" and cannot move up.
  (Composition{}, FocusUp, InCompositionFocus _ _   ) -> throwError CannotMoveUp
  (Sequence{}   , FocusUp, InSequenceFocus _ Nothing) -> throwError CannotMoveUp
  -- In case we have a parent, we try to move up within the child, or
  -- fall back to focus this parent.
  (Sequence _ sub, FocusUp, InSequenceFocus i (Just subFocus)) ->
    case modifyFocus (sub !! i) FocusUp subFocus of
      Left  CannotMoveUp -> pure (InSequenceFocus i Nothing)
      Left  err          -> throwError err
      Right f'           -> pure (InSequenceFocus i (Just f'))

  -- * Down
  (Sequence _ sub, FocusDown, InSequenceFocus i Nothing) -> case sub !! i of
    Sequence{} -> InSequenceFocus i . Just <$> modifyFocus
      s
      FocusDown
      (InSequenceFocus 0 Nothing)
    Composition{} ->
      pure (InSequenceFocus i (Just (InCompositionFocus Video 0)))
  (Composition _ _ audioClips, FocusDown, InCompositionFocus Video _)
    | not (null audioClips) -> pure (InCompositionFocus Audio 0)

  -- * Left
  (Sequence{}, FocusLeft, InSequenceFocus idx Nothing)
    | idx > 0   -> pure (InSequenceFocus (pred idx) Nothing)
    | otherwise -> throwError (OutOfBounds s e f)
  (Composition _ videoClips audioClips, FocusLeft, InCompositionFocus type' idx)
    | type' == Video && idx > 0 && idx < length videoClips
    -> pure (InCompositionFocus Video (pred idx))
    | type' == Audio && idx > 0 && idx < length audioClips
    -> pure (InCompositionFocus Audio (pred idx))
    | otherwise
    -> throwError (OutOfBounds s e f)

  -- * Right
  (Sequence _ sub, FocusRight, InSequenceFocus idx Nothing)
    | idx < (length sub - 1) -> pure (InSequenceFocus (succ idx) Nothing)
    | otherwise              -> throwError (OutOfBounds s e f)
  (Composition _ videoClips audioClips, FocusRight, InCompositionFocus type' idx)
    | type' == Video && idx >= 0 && idx < (length videoClips - 1)
    -> pure (InCompositionFocus Video (succ idx))
    | type' == Audio && idx >= 0 && idx < (length audioClips - 1)
    -> pure (InCompositionFocus Audio (succ idx))
    | otherwise
    -> throwError (OutOfBounds s e f)

  -- * Left or Right further down.
  (Sequence _ sub, _, InSequenceFocus idx (Just subFocus)) ->
    InSequenceFocus idx . Just <$> modifyFocus (sub !! idx) e subFocus

  _ -> throwError (UnhandledFocusModification s e f)
