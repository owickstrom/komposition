{-# OPTIONS_GHC -fno-warn-unticked-promoted-constructors #-}
{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE ExplicitForAll     #-}
{-# LANGUAGE GADTs              #-}
{-# LANGUAGE KindSignatures     #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE RankNTypes         #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeOperators      #-}
module FastCut.Focus where

import           FastCut.Prelude

import           Control.Lens         hiding (below)
import           Control.Monad.Except (throwError)
import           Data.Function        ((&))
import           Data.Maybe           (fromMaybe)

import           FastCut.Sequence

data FocusType
  = SequenceFocusType
  | ParallelFocusType
  | ClipFocusType

data Focus (t :: FocusType) where
  SequenceFocus
    :: Int -> Maybe (Focus ParallelFocusType) -> Focus SequenceFocusType
  ParallelFocus :: Int -> Maybe (Focus ClipFocusType) -> Focus ParallelFocusType
  ClipFocus :: MediaType -> Int -> Focus ClipFocusType

deriving instance Eq (Focus t)
deriving instance Show (Focus t)

data Focused
  = Focused
  | TransitivelyFocused
  | Blurred
  deriving (Eq, Show)

blurComposition :: Composition a t -> Composition Focused t
blurComposition = \case
  Timeline _ sub -> Timeline Blurred (map blurComposition sub)
  Sequence _ sub -> Sequence Blurred (map blurComposition sub)
  Parallel _ videoParts audioParts ->
    Parallel Blurred (map blurPart videoParts) (map blurPart audioParts)

blurPart :: SequencePart a t -> SequencePart Focused t
blurPart = setPartAnnotation Blurred

applyFocus :: Composition a t -> Focus ft -> Composition Focused t
applyFocus = go
  where
    go :: Composition a t -> Focus ft -> Composition Focused t
    go (Timeline _ sub) (SequenceFocus idx parallelFocus) =
      Timeline
        TransitivelyFocused
        (zipWith (applyAtSubComposition idx parallelFocus) sub [0 ..])
    go (Sequence _ sub) (ParallelFocus idx subFocus) =
      Sequence
        TransitivelyFocused
        (zipWith (applyAtSubComposition idx subFocus) sub [0 ..])
    go (Parallel _ videoParts audioParts) (ClipFocus focusPartType idx) =
      let focusInParts :: [SequencePart a mt] -> [SequencePart Focused mt]
          focusInParts clips = zipWith (focusPartAt idx) clips [0 ..]
          (videoParts', audioParts') =
            case focusPartType of
              Video -> (focusInParts videoParts, map blurPart audioParts)
              Audio -> (map blurPart videoParts, focusInParts audioParts)
      in Parallel TransitivelyFocused videoParts' audioParts'
    go c _ = blurComposition c
  -- Apply focus at the sub-composition specified by 'idx'.
    applyAtSubComposition ::
         Int
      -> Maybe (Focus ft)
      -> Composition a t
      -> Int
      -> Composition Focused t
    applyAtSubComposition idx subFocus subComposition subIdx
      | subIdx == idx =
        case subFocus of
          Just focus ->
            go
              (setCompositionAnnotation
                 TransitivelyFocused
                 (blurComposition subComposition))
              focus
          Nothing ->
            setCompositionAnnotation Focused (blurComposition subComposition)
      | otherwise = blurComposition subComposition
  -- Apply focus at the clip specified by 'idx'.
    focusPartAt :: Int -> SequencePart a mt -> Int -> SequencePart Focused mt
    focusPartAt idx clip clipIdx
      | clipIdx == idx = setPartAnnotation Focused clip
      | otherwise = blurPart clip

data FocusCommand = FocusUp | FocusDown | FocusLeft | FocusRight
  deriving (Eq, Show)

data FocusError
  = OutOfBounds
  | CannotMoveUp
  | CannotMoveDown
  | UnhandledFocusModification FocusCommand
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

compositionAt :: [Composition a t] -> Int -> Either FocusError (Composition a t)
compositionAt ss i =
  maybe (throwError OutOfBounds) pure (ss `atMay` i)

modifyFocus :: Composition a t -> FocusCommand -> Focus ft -> Either FocusError (Focus ft)
modifyFocus s e f = case (s, e, f) of

  -- Up

  (Timeline{}, FocusUp, SequenceFocus _ Nothing) ->
    throwError CannotMoveUp

  (Timeline _ seqs, FocusUp, SequenceFocus idx (Just parallelFocus)) -> do
    sequence' <- seqs `compositionAt` idx
    case modifyFocus sequence' FocusUp parallelFocus of
      Left  CannotMoveUp -> pure (SequenceFocus idx Nothing)
      Left  err          -> throwError err
      Right f'           -> pure (SequenceFocus idx (Just f'))

  -- Here we've hit a leaf and cannot move up.
  (Sequence{}   , FocusUp, ParallelFocus _ Nothing) ->
    throwError CannotMoveUp
  -- In case we have a parent, we try to move up within the child, or
  -- fall back to focus this parent.
  (Sequence _ sub, FocusUp, ParallelFocus i (Just subFocus)) -> do
    sub' <- sub `compositionAt` i
    case modifyFocus sub' FocusUp subFocus of
      Left  CannotMoveUp -> pure (ParallelFocus i Nothing)
      Left  err          -> throwError err
      Right f'           -> pure (ParallelFocus i (Just f'))

  -- We can move up from audio to video within a parallel.
  (Parallel _ videoParts audioParts, FocusUp, ClipFocus Audio i)
    -> case nearestPartIndexLeftOf audioParts i videoParts of
      Just i' -> pure (ClipFocus Video i')
      Nothing -> throwError OutOfBounds

  --  Here we've hit a focus "leaf" and cannot move up.
  (Parallel{}, FocusUp, ClipFocus _ _   ) -> throwError CannotMoveUp

  -- Down

  (Timeline _ seqs, FocusDown, SequenceFocus idx pf) -> do
    sequence'@(Sequence _ parallels) <- seqs `compositionAt` idx
    case pf of
      -- Down further within sequence.
      Just parallelFocus ->
        SequenceFocus idx . Just <$> modifyFocus sequence' FocusDown parallelFocus
      -- Down from sequence into parallel.
      Nothing
        | null parallels -> throwError CannotMoveDown
        | otherwise -> pure (SequenceFocus idx (Just (ParallelFocus 0 Nothing)))

  -- Down further within a focused parallel.
  (Sequence _ parallels, FocusDown, ParallelFocus idx (Just clipFocus)) -> do
    parallel <- parallels `compositionAt` idx
    ParallelFocus idx . Just <$> modifyFocus parallel FocusDown clipFocus

  -- Down into video track of focused composition.
  (Sequence _ parallels, FocusDown, ParallelFocus idx Nothing) -> do
    Parallel _ vs _ <- parallels `compositionAt` idx
    if null vs
      then throwError CannotMoveDown
      else pure (ParallelFocus idx (Just (ClipFocus Video 0)))

  -- We can move down from video to audio within a composition.
  (Parallel _ videoParts audioParts, FocusDown, ClipFocus Video i)
    -> case nearestPartIndexLeftOf videoParts i audioParts of
      Just i' -> pure (ClipFocus Audio i')
      Nothing -> throwError OutOfBounds

  -- We cannot move down any further when focusing an audio clip.
  (Parallel{}, FocusDown, ClipFocus Audio _) ->
      throwError CannotMoveDown

  -- Left
  (Timeline{}, FocusLeft, SequenceFocus idx Nothing)
    | idx > 0   -> pure (SequenceFocus (pred idx) Nothing)
    | otherwise -> throwError OutOfBounds
  (Sequence{}, FocusLeft, ParallelFocus idx Nothing)
    | idx > 0   -> pure (ParallelFocus (pred idx) Nothing)
    | otherwise -> throwError OutOfBounds
  (Parallel _ videoParts audioParts, FocusLeft, ClipFocus type' idx)
    | type' == Video && idx > 0 && idx < length videoParts
    -> pure (ClipFocus Video (pred idx))
    | type' == Audio && idx > 0 && idx < length audioParts
    -> pure (ClipFocus Audio (pred idx))
    | otherwise
    -> throwError OutOfBounds

  -- Right
  (Timeline _ sub, FocusRight, SequenceFocus idx Nothing)
    | idx < (length sub - 1) -> pure (SequenceFocus (succ idx) Nothing)
    | otherwise              -> throwError OutOfBounds
  (Sequence _ sub, FocusRight, ParallelFocus idx Nothing)
    | idx < (length sub - 1) -> pure (ParallelFocus (succ idx) Nothing)
    | otherwise              -> throwError OutOfBounds
  (Parallel _ videoParts audioParts, FocusRight, ClipFocus type' idx)
    | type' == Video && idx >= 0 && idx < (length videoParts - 1)
    -> pure (ClipFocus Video (succ idx))
    | type' == Audio && idx >= 0 && idx < (length audioParts - 1)
    -> pure (ClipFocus Audio (succ idx))
    | otherwise
    -> throwError OutOfBounds

  -- Left or right further down within timeline or sequence.
  (Timeline _ seqs, _, SequenceFocus idx (Just parallelFocus)) -> do
     seq' <- seqs `compositionAt` idx
     SequenceFocus idx . Just <$> modifyFocus seq' e parallelFocus
  (Sequence _ pars, _, ParallelFocus idx (Just clipFocus)) -> do
     par <- pars `compositionAt` idx
     ParallelFocus idx . Just <$> modifyFocus par e clipFocus

  _ -> throwError (UnhandledFocusModification e)

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
  -> Focus ft
  -> Composition a t
  -> m (Composition a t)
withParentOfM t@FocusedTraversal{..} f s =
  case (f, s) of
    (SequenceFocus idx Nothing, Timeline ann sub) ->
      onTimeline idx (Timeline ann sub)
    (SequenceFocus idx (Just subFocus), Timeline ann sub) ->
      sub
      & ix idx %%~ withParentOfM t subFocus
      & fmap (Timeline ann)
    (ParallelFocus idx Nothing, Sequence ann sub) ->
      onSequence idx (Sequence ann sub)
    (ParallelFocus idx (Just subFocus), Sequence ann sub) ->
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

atFocus :: Focus ft -> Composition a t -> Maybe (FocusedAt a)
atFocus f s =
  case (f, s) of
    (SequenceFocus idx Nothing, Timeline _ sub) ->
      FocusedSequence <$> sub `atMay` idx
    (SequenceFocus idx (Just subFocus), Timeline _ sub) ->
      atFocus subFocus =<< sub `atMay` idx
    (ParallelFocus idx Nothing, Sequence _ sub) ->
      FocusedParallel <$> sub `atMay` idx
    (ParallelFocus idx (Just subFocus), Sequence _ sub) ->
      atFocus subFocus =<< sub `atMay` idx
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
    Focus ft
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
    Focus ft
  -> Insertion a
  -> InsertPosition
  -> Composition a TimelineType
  -> Composition a TimelineType
insert_ f i p s = fromMaybe s (insert f i p s)
