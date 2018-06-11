{-# OPTIONS_GHC -fno-warn-unticked-promoted-constructors #-}
{-# LANGUAGE DataKinds      #-}
{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE GADTs          #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase     #-}
{-# LANGUAGE TypeOperators  #-}
module FastCut.Focus where

import           Control.Lens         hiding (below)
import           Control.Monad.Except (throwError)
import           Data.Function        ((&))

import           FastCut.Sequence

data Focus where
  SequenceFocus :: Focus
  SubFocus :: Int -> Focus -> Focus
  ClipFocus :: MediaType -> Int -> Focus
  deriving (Eq, Show)

data Focused
  = Focused
  | TransitivelyFocused
  | Blurred
  deriving (Eq, Show)

blurSequence :: Sequence a -> Sequence Focused
blurSequence = \case
  Sequence _ sub -> Sequence Blurred (map blurSequence sub)
  Composition _ videoParts audioParts ->
    Composition Blurred (map blurPart videoParts) (map blurPart audioParts)

blurPart :: SequencePart a t -> SequencePart Focused t
blurPart = setPartAnnotation Blurred

applyFocus :: Sequence a -> Focus -> Sequence Focused
applyFocus = go
  where
    go (Sequence _ sub) (SubFocus idx subFocus) =
      Sequence
        TransitivelyFocused
        (zipWith (applyAtSubSequence idx subFocus) sub [0 ..])
    go (Sequence _ sub) _ = Sequence Focused (map blurSequence sub)
    go (Composition _ videoParts audioParts) (ClipFocus focusPartType idx) =
      let focusInParts :: [SequencePart a mt] -> [SequencePart Focused mt]
          focusInParts clips = zipWith (focusPartAt idx) clips [0 ..]
          (videoParts', audioParts') =
            case focusPartType of
              Video -> (focusInParts videoParts, map blurPart audioParts)
              Audio -> (map blurPart videoParts, focusInParts audioParts)
      in Composition TransitivelyFocused videoParts' audioParts'
    go (Composition _ videoParts audioParts) _ =
      Composition Focused (map blurPart videoParts) (map blurPart audioParts)
  -- Apply focus at the sub-sequence specified by 'idx'.
    applyAtSubSequence idx subFocus subSequence subIdx
      | subIdx == idx = go subSequence subFocus
      | otherwise = blurSequence subSequence
  -- Apply focus at the clip specified by 'idx'.
    focusPartAt :: Int -> SequencePart a mt -> Int -> SequencePart Focused mt
    focusPartAt idx clip clipIdx
      | clipIdx == idx = setPartAnnotation Focused clip
      | otherwise = blurPart clip

data FocusEvent = FocusUp | FocusDown | FocusLeft | FocusRight
  deriving (Eq, Show)

data FocusError a
  = OutOfBounds (Sequence a) FocusEvent Focus
  | CannotMoveUp
  | UnhandledFocusModification (Sequence a) FocusEvent Focus
  deriving (Eq, Show)

indicesWithStartPoints :: [SequencePart a t] -> [(Int, Duration)]
indicesWithStartPoints clips =
    zip [0 .. (length clips - 1)] (scanl (\acc c -> durationOf c + acc) 0 clips)

nearestPartIndexLeftOf
  :: [SequencePart ann t] -> Int -> [SequencePart ann (InverseMediaType t)] -> Maybe Int
nearestPartIndexLeftOf focusedParts i blurredParts
  | i >= 0 && i < length focusedParts && not (null blurredParts)
  = let cutoffPoint = durationOf (take i focusedParts)
    in case
          takeWhile ((<= cutoffPoint) . snd)
                    (indicesWithStartPoints blurredParts)
        of
          []    -> Just 0
          below -> Just (fst (last below))
  | otherwise
  = Nothing

modifyFocus :: Sequence a -> FocusEvent -> Focus -> Either (FocusError a) Focus
modifyFocus s e f = case (s, e, f) of

  -- Up
  -- We can move up from audio to video within a composition.
  (Composition _ videoParts audioParts, FocusUp, ClipFocus Audio i)
    -> case nearestPartIndexLeftOf audioParts i videoParts of
      Just i' -> pure (ClipFocus Video i')
      Nothing -> throwError (OutOfBounds s e f)
  --  In these cases we've hit a focus "leaf" and cannot move up.
  (Composition{}, FocusUp, ClipFocus _ _   ) -> throwError CannotMoveUp
  (Sequence{}   , FocusUp, SubFocus _ SequenceFocus) -> throwError CannotMoveUp
  -- In case we have a parent, we try to move up within the child, or
  -- fall back to focus this parent.
  (Sequence _ sub, FocusUp, SubFocus i subFocus) ->
    case modifyFocus (sub !! i) FocusUp subFocus of
      Left  CannotMoveUp -> pure (SubFocus i SequenceFocus)
      Left  err          -> throwError err
      Right f'           -> pure (SubFocus i f')

  -- Down
  (Sequence _ sub, FocusDown, SubFocus i SequenceFocus) -> case sub !! i of
    Sequence{} -> SubFocus i <$> modifyFocus
      s
      FocusDown
      (SubFocus 0 SequenceFocus)
    Composition{} ->
      pure (SubFocus i (ClipFocus Video 0))
  -- We can move down from video to audio within a composition.
  (Composition _ videoParts audioParts, FocusDown, ClipFocus Video i)
    -> case nearestPartIndexLeftOf videoParts i audioParts of
      Just i' -> pure (ClipFocus Audio i')
      Nothing -> throwError (OutOfBounds s e f)

  -- Left
  (Sequence{}, FocusLeft, SubFocus idx SequenceFocus)
    | idx > 0   -> pure (SubFocus (pred idx) SequenceFocus)
    | otherwise -> throwError (OutOfBounds s e f)
  (Composition _ videoParts audioParts, FocusLeft, ClipFocus type' idx)
    | type' == Video && idx > 0 && idx < length videoParts
    -> pure (ClipFocus Video (pred idx))
    | type' == Audio && idx > 0 && idx < length audioParts
    -> pure (ClipFocus Audio (pred idx))
    | otherwise
    -> throwError (OutOfBounds s e f)

  -- Right
  (Sequence _ sub, FocusRight, SubFocus idx SequenceFocus)
    | idx < (length sub - 1) -> pure (SubFocus (succ idx) SequenceFocus)
    | otherwise              -> throwError (OutOfBounds s e f)
  (Composition _ videoParts audioParts, FocusRight, ClipFocus type' idx)
    | type' == Video && idx >= 0 && idx < (length videoParts - 1)
    -> pure (ClipFocus Video (succ idx))
    | type' == Audio && idx >= 0 && idx < (length audioParts - 1)
    -> pure (ClipFocus Audio (succ idx))
    | otherwise
    -> throwError (OutOfBounds s e f)

  -- Left or Right further down.
  (Sequence _ sub, _, SubFocus idx subFocus) ->
    SubFocus idx <$> modifyFocus (sub !! idx) e subFocus
  _ -> throwError (UnhandledFocusModification s e f)

withParentOf ::
   (Int -> Sequence a -> Sequence a)
  -> (Int -> [SequencePart a Video] -> [SequencePart a Video])
  -> (Int -> [SequencePart a Audio] -> [SequencePart a Audio])
  -> Focus
  -> Sequence a
  -> Sequence a
withParentOf onSequence onVideoParts onAudioParts f s =
  case (f, s) of
    (SubFocus idx SequenceFocus, Sequence ann sub) ->
      sub & ix idx %~ onSequence idx & Sequence ann
    (SubFocus idx subFocus, Sequence ann sub) ->
      sub & ix idx %~ go subFocus & Sequence ann
    (ClipFocus clipType idx, Composition ann videoParts audioParts) ->
      case clipType of
        Video -> Composition ann (onVideoParts idx videoParts) audioParts
        Audio -> Composition ann videoParts (onAudioParts idx audioParts)
    _ -> s
  where
    go = withParentOf onSequence onVideoParts onAudioParts

appendAt :: Focus -> Sequence () -> Sequence ()
appendAt =
  withParentOf onSequence onVideoClips onAudioClips
  where
    onSequence _ = id
    onVideoClips _ = id
    onAudioClips _ = id
