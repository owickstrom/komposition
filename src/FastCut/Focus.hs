{-# OPTIONS_GHC -fno-warn-unticked-promoted-constructors #-}
{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE ExplicitForAll     #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE GADTs              #-}
{-# LANGUAGE KindSignatures     #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE PolyKinds          #-}
{-# LANGUAGE RankNTypes         #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies       #-}
{-# LANGUAGE TypeOperators      #-}
module FastCut.Focus where

import           FastCut.Prelude

import           Control.Lens
import           Control.Monad.Except (throwError)
import qualified Data.List.NonEmpty   as NonEmpty

import           FastCut.Composition
import           FastCut.Duration
import           FastCut.MediaType

data FocusType
  = SequenceFocusType
  | ParallelFocusType
  | ClipFocusType

type family ToFocusType (ct :: CompositionType) :: FocusType where
  ToFocusType TimelineType = SequenceFocusType
  ToFocusType SequenceType = ParallelFocusType
  ToFocusType ParallelType = ClipFocusType

data Focus (t :: FocusType) where
  SequenceFocus
    :: Int -> Maybe (Focus ParallelFocusType) -> Focus SequenceFocusType
  ParallelFocus :: Int -> Maybe (Focus ClipFocusType) -> Focus ParallelFocusType
  ClipFocus :: MediaType -> Int -> Focus ClipFocusType

deriving instance Eq (Focus t)
deriving instance Show (Focus t)

instance Ord (Focus SequenceFocusType) where
  compare (SequenceFocus i1 f1) (SequenceFocus i2 f2) =
    case compare i1 i2 of
      EQ -> compare f1 f2
      o  -> o

instance Ord (Focus ParallelFocusType) where
  compare (ParallelFocus i1 f1) (ParallelFocus i2 f2) =
    case compare i1 i2 of
      EQ -> compare f1 f2
      o  -> o

instance Ord (Focus ClipFocusType) where
  compare (ClipFocus Video i1) (ClipFocus Video i2) = compare i1 i2
  compare (ClipFocus Audio i1) (ClipFocus Audio i2) = compare i1 i2
  compare (ClipFocus mt1 _) (ClipFocus mt2 _)       = compare mt1 mt2


focusType :: Focus t -> FocusType
focusType = \case
  SequenceFocus _ Nothing -> SequenceFocusType
  SequenceFocus _ (Just f) -> focusType f
  ParallelFocus _ Nothing -> ParallelFocusType
  ParallelFocus _ (Just f) -> focusType f
  ClipFocus{} -> ClipFocusType

data FocusCommand = FocusUp | FocusDown | FocusLeft | FocusRight
  deriving (Eq, Show, Ord, Enum, Bounded)

data FocusError
  = OutOfBounds
  | CannotMoveUp
  | CannotMoveDown
  | UnhandledFocusModification FocusCommand
  deriving (Show, Eq)

indicesWithStartPoints :: [CompositionPart a t] -> [(Int, Duration)]
indicesWithStartPoints clips =
  zip [0 .. (length clips - 1)] (scanl (\acc c -> durationOf c + acc) 0 clips)

nearestPartIndexLeftOf
  :: [CompositionPart t ann]
  -> Int
  -> [CompositionPart (InverseMediaType t) ann]
  -> Maybe Int
nearestPartIndexLeftOf focusedParts i blurredParts
  | i >= 0 && i < length focusedParts && not (null blurredParts)
  = let cutoffPoint = foldMap durationOf (take i focusedParts)
        below'      = takeWhile ((<= cutoffPoint) . snd)
                                (indicesWithStartPoints blurredParts)
    in  (fst <$> lastMay below') <|> Just 0
  | otherwise
  = Nothing

compositionAt
  :: NonEmpty (Composition a t) -> Int -> Either FocusError (Composition a t)
compositionAt ss i = maybe (throwError OutOfBounds) pure (toList ss `atMay` i)

modifyFocus
  :: Composition a t -> FocusCommand -> Focus ft -> Either FocusError (Focus ft)
modifyFocus s e f = case (s, e, f) of

  -- Up

  (Timeline{}, FocusUp, SequenceFocus _ Nothing) -> throwError CannotMoveUp

  (Timeline seqs, FocusUp, SequenceFocus idx (Just parallelFocus)) -> do
    sequence' <- seqs `compositionAt` idx
    case modifyFocus sequence' FocusUp parallelFocus of
      Left  CannotMoveUp -> pure (SequenceFocus idx Nothing)
      Left  err          -> throwError err
      Right f'           -> pure (SequenceFocus idx (Just f'))

  -- Here we've hit a leaf and cannot move up.
  (Sequence{}, FocusUp, ParallelFocus _ Nothing) -> throwError CannotMoveUp
  -- In case we have a parent, we try to move up within the child, or
  -- fall back to focus this parent.
  (Sequence _ sub, FocusUp, ParallelFocus i (Just subFocus)) -> do
    sub' <- sub `compositionAt` i
    case modifyFocus sub' FocusUp subFocus of
      Left  CannotMoveUp -> pure (ParallelFocus i Nothing)
      Left  err          -> throwError err
      Right f'           -> pure (ParallelFocus i (Just f'))

  (Parallel _ videoParts audioParts, FocusUp, ClipFocus Audio i)
    -- We can move up from audio to video within a parallel if there are video parts.
    | not (null videoParts) ->
      case nearestPartIndexLeftOf audioParts i videoParts of
        Just i' -> pure (ClipFocus Video i')
        Nothing -> throwError OutOfBounds
    -- Otherwise we'll move further up.
    | otherwise -> throwError CannotMoveUp

  --  Here we've hit a focus "leaf" and cannot move up.
  (Parallel{}     , FocusUp  , ClipFocus Video _       ) -> throwError CannotMoveUp

  -- Down

  (Timeline seqs, FocusDown, SequenceFocus idx pf) -> do
    sequence'@(Sequence _ parallels) <- seqs `compositionAt` idx
    case pf of
      -- Down further within sequence.
      Just parallelFocus ->
        SequenceFocus idx
          .   Just
          <$> modifyFocus sequence' FocusDown parallelFocus
      -- Down from sequence into parallel.
      Nothing
        | null parallels -> throwError CannotMoveDown
        | otherwise -> pure (SequenceFocus idx (Just (ParallelFocus 0 Nothing)))

  -- Down further within a focused parallel.
  (Sequence _ parallels, FocusDown, ParallelFocus idx (Just clipFocus)) -> do
    parallel <- parallels `compositionAt` idx
    ParallelFocus idx . Just <$> modifyFocus parallel FocusDown clipFocus

  (Sequence _ parallels, FocusDown, ParallelFocus idx Nothing) -> do
    Parallel _ vs as <- parallels `compositionAt` idx

    case (vs, as) of
      -- Down into video track of focused parallel.
      (_ : _, _    ) -> pure (ParallelFocus idx (Just (ClipFocus Video 0)))
      -- Down into audio track, past empty video track, of focused parallel.
      ([]   , _ : _) -> pure (ParallelFocus idx (Just (ClipFocus Audio 0)))
      _              -> throwError CannotMoveDown

  -- We can move down from video to audio within a composition.
  (Parallel _ videoParts audioParts, FocusDown, ClipFocus Video i) ->
    case nearestPartIndexLeftOf videoParts i audioParts of
      Just i' -> pure (ClipFocus Audio i')
      Nothing -> throwError OutOfBounds

  -- We cannot move down any further when focusing an audio clip.
  (Parallel{}, FocusDown, ClipFocus Audio _) -> throwError CannotMoveDown

  -- Left
  (Timeline{}, FocusLeft, SequenceFocus idx Nothing)
    | idx > 0   -> pure (SequenceFocus (pred idx) Nothing)
    | otherwise -> throwError OutOfBounds
  (Sequence{}, FocusLeft, ParallelFocus idx Nothing)
    | idx > 0   -> pure (ParallelFocus (pred idx) Nothing)
    | otherwise -> throwError OutOfBounds
  (Parallel _ videoParts audioParts, FocusLeft, ClipFocus type' idx)
    | type' == Video && idx > 0 && idx < length videoParts -> pure
      (ClipFocus Video (pred idx))
    | type' == Audio && idx > 0 && idx < length audioParts -> pure
      (ClipFocus Audio (pred idx))
    | otherwise -> throwError OutOfBounds

  -- Right
  (Timeline sub, FocusRight, SequenceFocus idx Nothing)
    | idx < (length sub - 1) -> pure (SequenceFocus (succ idx) Nothing)
    | otherwise              -> throwError OutOfBounds
  (Sequence _ sub, FocusRight, ParallelFocus idx Nothing)
    | idx < (length sub - 1) -> pure (ParallelFocus (succ idx) Nothing)
    | otherwise              -> throwError OutOfBounds
  (Parallel _ videoParts audioParts, FocusRight, ClipFocus type' idx)
    | type' == Video && idx >= 0 && idx < (length videoParts - 1) -> pure
      (ClipFocus Video (succ idx))
    | type' == Audio && idx >= 0 && idx < (length audioParts - 1) -> pure
      (ClipFocus Audio (succ idx))
    | otherwise -> throwError OutOfBounds

  -- Left or right further down within timeline or sequence.
  (Timeline seqs, _, SequenceFocus idx (Just parallelFocus)) -> do
    seq' <- seqs `compositionAt` idx
    SequenceFocus idx . Just <$> modifyFocus seq' e parallelFocus
  (Sequence _ pars, _, ParallelFocus idx (Just clipFocus)) -> do
    par <- pars `compositionAt` idx
    ParallelFocus idx . Just <$> modifyFocus par e clipFocus

  _ -> throwError (UnhandledFocusModification e)

data FocusedTraversal (f :: * -> *) a = FocusedTraversal
  { mapSequence   :: Composition SequenceType a -> f (Composition SequenceType a)
  , mapParallel   :: Composition ParallelType a -> f (Composition ParallelType a)
  , mapCompositionPart :: forall mt. SMediaType mt -> CompositionPart mt a -> f (CompositionPart mt a)
  }

mapAtFocus
  :: Applicative f
  => Focus ft
  -> FocusedTraversal f a
  -> Composition t a
  -> f (Composition t a)
mapAtFocus focus t@FocusedTraversal {..} s = case (focus, s) of
  (SequenceFocus idx Nothing, Timeline sub) ->
    Timeline <$> mapAt idx mapSequence sub
  (SequenceFocus idx (Just subFocus), Timeline sub) ->
    Timeline <$> mapAt idx (mapAtFocus subFocus t) sub
  (ParallelFocus idx Nothing, Sequence ann sub) ->
    Sequence ann <$> mapAt idx mapParallel sub
  (ParallelFocus idx (Just subFocus), Sequence ann sub) ->
    Sequence ann <$> mapAt idx (mapAtFocus subFocus t) sub
  (ClipFocus clipType idx, Parallel ann videoParts audioParts) ->
    case clipType of
      Video ->
        Parallel ann
          <$> (videoParts & ix idx %%~ mapCompositionPart SVideo)
          <*> pure audioParts
      Audio ->
        Parallel ann videoParts
          <$> (audioParts & ix idx %%~ mapCompositionPart SAudio)
  _ -> pure s
 where
  mapAt :: Applicative f => Int -> (a -> f a) -> NonEmpty a -> f (NonEmpty a)
  mapAt idx f xs = toList xs & ix idx %%~ f <&> NonEmpty.fromList

data FocusedAt a
  = FocusedSequence (Composition SequenceType a)
  | FocusedParallel (Composition ParallelType a)
  | FocusedVideoPart (CompositionPart Video a)
  | FocusedAudioPart (CompositionPart Audio a)
  deriving (Show, Eq)

atFocus :: Focus ft -> Composition t a -> Maybe (FocusedAt a)
atFocus focus comp = execState (mapAtFocus focus traversal comp) Nothing
 where
  remember
    :: forall c (t :: k) a
     . (c t a -> FocusedAt a)
    -> c t a
    -> State (Maybe (FocusedAt a)) (c t a)
  remember focused x = put (Just (focused x)) >> pure x
  traversal = FocusedTraversal
    { mapSequence        = remember FocusedSequence
    , mapParallel        = remember FocusedParallel
    , mapCompositionPart = \case
      SVideo -> remember FocusedVideoPart
      SAudio -> remember FocusedAudioPart
    }

data FirstCompositionPart a
  = FirstVideoPart (CompositionPart Video a)
  | FirstAudioPart (CompositionPart Audio a)

firstCompositionPart
  :: Focus ft -> Composition t a -> Maybe (FirstCompositionPart a)
firstCompositionPart f s = atFocus f s >>= \case
  FocusedSequence  s' -> firstInSequence s'
  FocusedParallel  p  -> firstInParallel p
  FocusedVideoPart v  -> Just (FirstVideoPart v)
  FocusedAudioPart a  -> Just (FirstAudioPart a)
 where
  firstInSequence
    :: Composition SequenceType a -> Maybe (FirstCompositionPart a)
  firstInSequence (Sequence _ ps) = firstInParallel (NonEmpty.head ps)
  firstInParallel
    :: Composition ParallelType a -> Maybe (FirstCompositionPart a)
  firstInParallel = \case
    Parallel _ (v : _) _       -> Just (FirstVideoPart v)
    Parallel _ []      (a : _) -> Just (FirstAudioPart a)
    _                          -> Nothing
