{-# OPTIONS_GHC -fno-warn-unticked-promoted-constructors #-}
{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE ExplicitForAll  #-}
{-# LANGUAGE GADTs           #-}
{-# LANGUAGE KindSignatures  #-}
{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeOperators   #-}
module FastCut.Focus where

import           Control.Lens         hiding (below)
import           Control.Monad
import           Control.Monad.Except (throwError)
import           Data.Function        ((&))
import           Data.Maybe           (fromMaybe)

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

data FocusedTraversal m a = FocusedTraversal
  { onSequence   :: Int -> Sequence a -> m (Sequence a)
  , onVideoParts :: Int -> [SequencePart a Video] -> m [SequencePart a Video]
  , onAudioParts :: Int -> [SequencePart a Audio] -> m [SequencePart a Audio]
  }

focusedTraversal :: Applicative m => FocusedTraversal m a
focusedTraversal =
  FocusedTraversal
    (const pure)
    (const pure)
    (const pure)

withParentOfM ::
  (MonadPlus m, Monad m)
  => FocusedTraversal m a
  -> Focus
  -> Sequence a
  -> m (Sequence a)
withParentOfM t@FocusedTraversal{..} f s =
  case (f, s) of
    (SubFocus idx SequenceFocus, Sequence ann sub) ->
      onSequence idx (Sequence ann sub)
    (SubFocus idx subFocus, Sequence ann sub) ->
      sub
      & ix idx %%~ withParentOfM t subFocus
      & fmap (Sequence ann)
    (ClipFocus clipType idx, Composition ann videoParts audioParts) ->
      case clipType of
        Video -> onVideoParts idx videoParts >>= \vs ->
          pure (Composition ann vs audioParts)
        Audio -> onAudioParts idx audioParts >>= \as ->
          pure (Composition ann videoParts as)
    _ -> mzero

data FocusedAt a
  = FocusedSequence (Sequence a)
  | FocusedVideoPart (SequencePart a Video)
  | FocusedAudioPart (SequencePart a Audio)

atFocus :: Focus -> Sequence a -> Maybe (FocusedAt a)
atFocus f s =
  case (f, s) of
    (SequenceFocus, Sequence{}) ->
      pure (FocusedSequence s)
    (SubFocus idx SequenceFocus, Sequence _ sub) ->
      pure (FocusedSequence (sub !! idx))
    (SubFocus idx subFocus, Sequence _ sub) ->
      atFocus subFocus (sub !! idx)
    (ClipFocus clipType idx, Composition _ videoParts audioParts) ->
      case clipType of
        Video -> pure (FocusedVideoPart (videoParts !! idx))
        Audio -> pure (FocusedAudioPart (audioParts !! idx))
    _ -> mzero

insertAt :: Int -> a -> [a] -> [a]
insertAt i x xs =
  let (before, after) = splitAt i xs
  in before <> (x : after)

appendAt ::
    Focus
  -> Either (Sequence a) (SequencePart a mt)
  -> Sequence a
  -> Maybe (Sequence a)
appendAt focus (Left sequence') parent =
  let
    onSequence i = \case
      Sequence ann children' -> pure (Sequence ann (insertAt (succ i) sequence' children'))
      _  -> mzero
    traversal = focusedTraversal { onSequence = onSequence }
  in withParentOfM traversal focus parent
appendAt focus (Right part) parent = withParentOfM traversal focus parent
  where
    insertVideo v i = pure . insertAt (succ i) v
    insertAudio a i = pure . insertAt (succ i) a
    traversal =
      case part of
        v@(Clip VideoClip {}) -> focusedTraversal {onVideoParts = insertVideo v}
        a@(Clip AudioClip {}) -> focusedTraversal {onAudioParts = insertAudio a}
        Gap ann dur ->
          focusedTraversal
          { onVideoParts = insertVideo (Gap ann dur)
          , onAudioParts = insertAudio (Gap ann dur)
          }

appendAt' ::
    Focus
  -> Either (Sequence a) (SequencePart a mt)
  -> Sequence a
  -> Sequence a
appendAt' f p s = fromMaybe s $ appendAt f p s
