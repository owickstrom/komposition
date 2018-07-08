{-# LANGUAGE RankNTypes      #-}
{-# OPTIONS_GHC -fno-warn-unticked-promoted-constructors #-}
{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE ExplicitForAll  #-}
{-# LANGUAGE GADTs           #-}
{-# LANGUAGE KindSignatures  #-}
{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeOperators   #-}
module FastCut.Focus where

import           FastCut.Prelude

import           Control.Lens         hiding (below)
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

blurSequence :: Composition a t -> Composition Focused t
blurSequence = \case
  Timeline _ sub -> Timeline Blurred (map blurSequence sub)
  Sequence _ sub -> Sequence Blurred (map blurSequence sub)
  Parallel _ videoParts audioParts ->
    Parallel Blurred (map blurPart videoParts) (map blurPart audioParts)

blurPart :: SequencePart a t -> SequencePart Focused t
blurPart = setPartAnnotation Blurred

applyFocus :: Composition a t -> Focus -> Composition Focused t
applyFocus = go
  where
    go :: Composition a t -> Focus -> Composition Focused t
    go (Timeline _ sub) (SubFocus idx subFocus) =
      Timeline
        TransitivelyFocused
        (zipWith (applyAtSubSequence idx subFocus) sub [0 ..])
    go (Timeline _ sub) _ = Timeline Blurred (map blurSequence sub)
    go (Sequence _ sub) (SubFocus idx subFocus) =
      Sequence
        TransitivelyFocused
        (zipWith (applyAtSubSequence idx subFocus) sub [0 ..])
    go (Sequence _ sub) _ = Sequence Focused (map blurSequence sub)
    go (Parallel _ videoParts audioParts) (ClipFocus focusPartType idx) =
      let focusInParts :: [SequencePart a mt] -> [SequencePart Focused mt]
          focusInParts clips = zipWith (focusPartAt idx) clips [0 ..]
          (videoParts', audioParts') =
            case focusPartType of
              Video -> (focusInParts videoParts, map blurPart audioParts)
              Audio -> (map blurPart videoParts, focusInParts audioParts)
      in Parallel TransitivelyFocused videoParts' audioParts'
    go (Parallel _ videoParts audioParts) _ =
      Parallel Focused (map blurPart videoParts) (map blurPart audioParts)
  -- Apply focus at the sub-sequence specified by 'idx'.
    applyAtSubSequence ::
         Int -> Focus -> Composition a t -> Int -> Composition Focused t
    applyAtSubSequence idx subFocus subSequence subIdx
      | subIdx == idx = go subSequence subFocus
      | otherwise = blurSequence subSequence
  -- Apply focus at the clip specified by 'idx'.
    focusPartAt :: Int -> SequencePart a mt -> Int -> SequencePart Focused mt
    focusPartAt idx clip clipIdx
      | clipIdx == idx = setPartAnnotation Focused clip
      | otherwise = blurPart clip

data FocusCommand = FocusUp | FocusDown | FocusLeft | FocusRight
  deriving (Eq, Show)

data FocusError a
  = OutOfBounds FocusCommand Focus
  | IndexError
  | CannotMoveUp
  | CannotMoveDown
  | UnhandledFocusModification FocusCommand Focus
  deriving (Show, Eq)

indicesWithStartPoints :: [SequencePart a t] -> [(Int, Duration)]
indicesWithStartPoints clips =
    zip [0 .. (length clips - 1)] (scanl (\acc c -> durationOf c + acc) 0 clips)

nearestPartIndexLeftOf
  :: [SequencePart ann t] -> Int -> [SequencePart ann (InverseMediaType t)] -> Maybe Int
nearestPartIndexLeftOf focusedParts i blurredParts
  | i >= 0 && i < length focusedParts && not (null blurredParts)
  = let cutoffPoint = durationOf (take i focusedParts)
        below = takeWhile ((<= cutoffPoint) . snd)
                    (indicesWithStartPoints blurredParts)
    in (fst <$> lastMay below) <|> Just 0
  | otherwise
  = Nothing

sequenceAt :: [Composition a t] -> Int -> Either (FocusError a) (Composition a t)
sequenceAt ss i =
  maybe (throwError IndexError) pure (ss `atMay` i)

modifyFocus :: Composition a t -> FocusCommand -> Focus -> Either (FocusError a) Focus
modifyFocus s e f = case (s, e, f) of

  -- Up
  -- We can move up from audio to video within a parallel.
  (Parallel _ videoParts audioParts, FocusUp, ClipFocus Audio i)
    -> case nearestPartIndexLeftOf audioParts i videoParts of
      Just i' -> pure (ClipFocus Video i')
      Nothing -> throwError (OutOfBounds e f)

  --  In these cases we've hit a focus "leaf" and cannot move up.
  (Parallel{}, FocusUp, ClipFocus _ _   ) -> throwError CannotMoveUp
  (Sequence{}   , FocusUp, SubFocus _ SequenceFocus) -> throwError CannotMoveUp
  -- In case we have a parent, we try to move up within the child, or
  -- fall back to focus this parent.
  (Sequence _ sub, FocusUp, SubFocus i subFocus) -> do
    sub' <- sub `sequenceAt` i
    case modifyFocus sub' FocusUp subFocus of
      Left  CannotMoveUp -> pure (SubFocus i SequenceFocus)
      Left  err          -> throwError err
      Right f'           -> pure (SubFocus i f')
  (Timeline _ sub, FocusUp, SubFocus i SequenceFocus) ->
    throwError CannotMoveUp
  -- TODO: This is the same as the Sequence case above, extract to helper
  -- function.
  (Timeline _ sub, FocusUp, SubFocus i subFocus) -> do
    sub' <- sub `sequenceAt` i
    case modifyFocus sub' FocusUp subFocus of
      Left  CannotMoveUp -> pure (SubFocus i SequenceFocus)
      Left  err          -> throwError err
      Right f'           -> pure (SubFocus i f')

  -- Down

  -- Down into focused sequence.
  (Sequence _ sub, FocusDown, SequenceFocus)
    | null sub -> throwError CannotMoveDown
    | otherwise ->
      sub `sequenceAt` 0 >>= \case
        Parallel{} -> pure (SubFocus 0 (ClipFocus Video 0))

  -- Down into video track of focused composition.
  (Parallel _ videoParts _, FocusDown, SequenceFocus)
    | null videoParts -> throwError CannotMoveDown
    | otherwise ->
      pure (ClipFocus Video 0)

  -- Move down further within a sequence.
  (Sequence _ sub, FocusDown, SubFocus i subFocus)
      -- We cannot move down into an empty sequence.
    | null sub -> throwError CannotMoveDown
    | otherwise -> do
      sub' <- sub `sequenceAt` i
      SubFocus i <$> modifyFocus
        sub'
        FocusDown
        subFocus

  -- We can move down from video to audio within a composition.
  (Parallel _ videoParts audioParts, FocusDown, ClipFocus Video i)
    -> case nearestPartIndexLeftOf videoParts i audioParts of
      Just i' -> pure (ClipFocus Audio i')
      Nothing -> throwError (OutOfBounds e f)

  -- Left
  (Timeline{}, FocusLeft, SubFocus idx SequenceFocus)
    | idx > 0   -> pure (SubFocus (pred idx) SequenceFocus)
    | otherwise -> throwError (OutOfBounds e f)
  (Sequence{}, FocusLeft, SubFocus idx SequenceFocus)
    | idx > 0   -> pure (SubFocus (pred idx) SequenceFocus)
    | otherwise -> throwError (OutOfBounds e f)
  (Parallel _ videoParts audioParts, FocusLeft, ClipFocus type' idx)
    | type' == Video && idx > 0 && idx < length videoParts
    -> pure (ClipFocus Video (pred idx))
    | type' == Audio && idx > 0 && idx < length audioParts
    -> pure (ClipFocus Audio (pred idx))
    | otherwise
    -> throwError (OutOfBounds e f)

  -- Right
  (Sequence _ sub, FocusRight, SubFocus idx SequenceFocus)
    | idx < (length sub - 1) -> pure (SubFocus (succ idx) SequenceFocus)
    | otherwise              -> throwError (OutOfBounds e f)
  (Parallel _ videoParts audioParts, FocusRight, ClipFocus type' idx)
    | type' == Video && idx >= 0 && idx < (length videoParts - 1)
    -> pure (ClipFocus Video (succ idx))
    | type' == Audio && idx >= 0 && idx < (length audioParts - 1)
    -> pure (ClipFocus Audio (succ idx))
    | otherwise
    -> throwError (OutOfBounds e f)

  -- Further down.
  (Timeline _ sub, _, SubFocus idx subFocus) -> do
    sequence' <- sub `sequenceAt` idx
    SubFocus idx <$> modifyFocus sequence' e subFocus
  (Sequence _ sub, _, SubFocus idx subFocus) -> do
    sub' <- sub `sequenceAt` idx
    SubFocus idx <$> modifyFocus sub' e subFocus

  _ -> throwError (UnhandledFocusModification e f)

data FocusedTraversal m a = FocusedTraversal
  { onTimeline :: Int -> Composition a TimelineType  -> m (Composition a TimelineType)
  , onSequence   :: Int -> Composition a SequenceType -> m (Composition a SequenceType)
  , onVideoParts :: Int -> [SequencePart a Video] -> m [SequencePart a Video]
  , onAudioParts :: Int -> [SequencePart a Audio] -> m [SequencePart a Audio]
  }

focusedTraversal :: Applicative m => FocusedTraversal m a
focusedTraversal =
  FocusedTraversal
    (const pure)
    (const pure)
    (const pure)
    (const pure)

withParentOfM ::
  (MonadPlus m, Monad m)
  => FocusedTraversal m a
  -> Focus
  -> Composition a t
  -> m (Composition a t)
withParentOfM t@FocusedTraversal{..} f s =
  case (f, s) of
    (SubFocus idx SequenceFocus, Timeline ann sub) ->
      onTimeline idx (Timeline ann sub)
    (SubFocus idx subFocus, Timeline ann sub) ->
      sub
      & ix idx %%~ withParentOfM t subFocus
      & fmap (Timeline ann)
    (SubFocus idx SequenceFocus, Sequence ann sub) ->
      onSequence idx (Sequence ann sub)
    (SubFocus idx subFocus, Sequence ann sub) ->
      sub
      & ix idx %%~ withParentOfM t subFocus
      & fmap (Sequence ann)
    (ClipFocus clipType idx, Parallel ann videoParts audioParts) ->
      case clipType of
        Video -> onVideoParts idx videoParts >>= \vs ->
          pure (Parallel ann vs audioParts)
        Audio -> onAudioParts idx audioParts >>= \as ->
          pure (Parallel ann videoParts as)
    _ -> mzero

data FocusedAt a
  = FocusedSequence (Composition a SequenceType)
  | FocusedParallel (Composition a ParallelType)
  | FocusedVideoPart (SequencePart a Video)
  | FocusedAudioPart (SequencePart a Audio)
  deriving (Show, Eq)

atFocus :: Focus -> Composition a t -> Maybe (FocusedAt a)
atFocus f s =
  case (f, s) of
    (SubFocus idx SequenceFocus, Timeline _ sub) ->
      FocusedSequence <$> sub `atMay` idx
    (SubFocus idx subFocus, Timeline _ sub) ->
      atFocus subFocus =<< sub `atMay` idx
    (SequenceFocus, Sequence {}) -> pure (FocusedSequence s)
    (SubFocus idx SequenceFocus, Sequence _ sub) ->
      FocusedParallel <$> sub `atMay` idx
    (SubFocus _ SubFocus{}, Sequence {}) -> mzero
    (ClipFocus clipType idx, Parallel _ videoParts audioParts) ->
      case clipType of
        Video -> FocusedVideoPart <$> videoParts `atMay` idx
        Audio -> FocusedAudioPart <$> audioParts `atMay` idx
    _ -> mzero

insertAt :: Int -> a -> [a] -> [a]
insertAt i x xs =
  let (before, after) = splitAt i xs
  in before <> (x : after)

data InsertPosition
  = LeftMost
  | LeftOf
  | RightOf
  | RightMost

data Insertion a
  = InsertSequence (Composition a SequenceType)
  | InsertParallel (Composition a ParallelType)
  | InsertVideoPart (SequencePart a Video)
  | InsertAudioPart (SequencePart a Audio)

insert ::
    Focus
  -> Insertion a
  -> InsertPosition
  -> Composition a TimelineType
  -> Maybe (Composition a TimelineType)
insert focus insertion position parent =
  traversal >>= \t -> withParentOfM t focus parent
  where
    traversal =
      case (insertion, position) of
        (InsertSequence sequence', RightOf) ->
          let onTimeline i (Timeline ann children') =
                pure (Timeline ann (insertAt (succ i) sequence' children'))
          in pure (focusedTraversal {onTimeline = onTimeline})
        (InsertParallel parallel, RightOf) ->
          let onSequence i =
                \case
                  Sequence ann children' ->
                    pure (Sequence ann (insertAt (succ i) parallel children'))
          in pure (focusedTraversal {onSequence = onSequence})
        (InsertVideoPart part, RightOf) ->
          pure
            (focusedTraversal
             {onVideoParts = \i -> pure . insertAt (succ i) part})
        (InsertAudioPart part, RightOf) ->
          pure (focusedTraversal {onAudioParts = \i -> pure . insertAt (succ i) part})
        (_, _) -> mzero

insert_ ::
    Focus
  -> Insertion a
  -> InsertPosition
  -> Composition a TimelineType
  -> Composition a TimelineType
insert_ f i p s = fromMaybe s (insert f i p s)
