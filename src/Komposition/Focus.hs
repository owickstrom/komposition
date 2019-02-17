{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE ExplicitForAll        #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
module Komposition.Focus where

import           Komposition.Prelude

import           Control.Lens
import           Control.Monad.Except    (throwError)
import qualified Data.List.NonEmpty      as NonEmpty

import           Komposition.Composition
import           Komposition.Duration
import           Komposition.MediaType

data FocusType
  = SequenceFocusType
  | ParallelFocusType
  | TrackFocusType
  | ClipFocusType

type family ToFocusType (ct :: * -> *) :: FocusType where
  ToFocusType Timeline = 'SequenceFocusType
  ToFocusType Sequence = 'ParallelFocusType
  ToFocusType Parallel = 'TrackFocusType
  ToFocusType VideoTrack = 'ClipFocusType
  ToFocusType AudioTrack = 'ClipFocusType

data SequenceFocus = SequenceFocus Int (Maybe ParallelFocus)
  deriving (Eq, Show, Ord, Generic)

data ParallelFocus = ParallelFocus Int (Maybe TrackFocus)
  deriving (Eq, Show, Ord, Generic)

data TrackFocus = TrackFocus MediaType (Maybe ClipFocus)
  deriving (Eq, Show, Ord, Generic)

newtype ClipFocus = ClipFocus Int
  deriving (Eq, Show, Ord, Generic)

type family Focus (t :: FocusType) where
  Focus 'SequenceFocusType = SequenceFocus
  Focus 'ParallelFocusType = ParallelFocus
  Focus 'TrackFocusType = TrackFocus
  Focus 'ClipFocusType = ClipFocus

class HasLeafFocusIndexLens focus where
  leafFocusIndex :: Applicative f => (Int -> f Int) -> focus -> f focus

instance HasLeafFocusIndexLens SequenceFocus where
  leafFocusIndex f (SequenceFocus i Nothing) = flip SequenceFocus Nothing <$> f i
  leafFocusIndex f (SequenceFocus i (Just pf)) = SequenceFocus i . Just <$> leafFocusIndex f pf

instance HasLeafFocusIndexLens ParallelFocus where
  leafFocusIndex f (ParallelFocus i Nothing) =   flip ParallelFocus Nothing <$> f i
  leafFocusIndex f (ParallelFocus i (Just tf)) = ParallelFocus i . Just <$> leafFocusIndex f tf

instance HasLeafFocusIndexLens TrackFocus where
  leafFocusIndex f (TrackFocus mt cf) =   TrackFocus mt <$> maybe (pure Nothing) (fmap Just <$> leafFocusIndex f) cf

instance HasLeafFocusIndexLens ClipFocus where
  leafFocusIndex f (ClipFocus i ) = ClipFocus <$> f i

class ChangeFocusUp focus where
  changeFocusUp :: focus -> Maybe focus

instance ChangeFocusUp SequenceFocus where
  changeFocusUp (SequenceFocus _  Nothing)   = mzero
  changeFocusUp (SequenceFocus i  (Just pf)) = pure (SequenceFocus i (changeFocusUp pf))

instance ChangeFocusUp ParallelFocus where
  changeFocusUp (ParallelFocus _  Nothing)   = mzero
  changeFocusUp (ParallelFocus i  (Just tf)) = pure (ParallelFocus i (changeFocusUp tf))

instance ChangeFocusUp TrackFocus where
  changeFocusUp (TrackFocus    _  Nothing)   = mzero
  changeFocusUp (TrackFocus    mt (Just cf)) = pure (TrackFocus mt (changeFocusUp cf))

instance ChangeFocusUp ClipFocus where
  changeFocusUp (ClipFocus _)                = mzero

focusType :: SequenceFocus -> FocusType
focusType = \case
  SequenceFocus _ Nothing  -> SequenceFocusType
  SequenceFocus _ (Just pf) ->
    case pf of
      ParallelFocus _ Nothing  -> ParallelFocusType
      ParallelFocus _ (Just tf) ->
        case tf of
          TrackFocus _ Nothing  -> TrackFocusType
          TrackFocus _ (Just _) -> ClipFocusType

data FocusCommand = FocusUp | FocusDown | FocusLeft | FocusRight
  deriving (Eq, Show, Ord, Enum, Bounded)

data FocusError
  = OutOfBounds
  | CannotMove FocusCommand
  | UnhandledFocusModification FocusCommand
  deriving (Show, Eq)

indicesWithStartPoints :: HasDuration a => [a] -> [(Int, Duration)]
indicesWithStartPoints clips =
  zip [0 .. (length clips - 1)] (scanl (\acc c -> durationOf AdjustedDuration c + acc) 0 clips)

nearestPartIndexLeftOf
  :: (HasDuration a, HasDuration b) => [a] -> Int -> [b] -> Maybe Int
nearestPartIndexLeftOf focusedParts i blurredParts
  | i >= 0 && i < length focusedParts && not (null blurredParts)
  = let cutoffPoint = foldMap (durationOf AdjustedDuration) (take i focusedParts)
        below'      = takeWhile ((<= cutoffPoint) . snd)
                                (indicesWithStartPoints blurredParts)
    in  (fst <$> lastMay below') <|> Just 0
  | otherwise
  = Nothing

compositionAt :: NonEmpty (t a) -> Int -> Either FocusError (t a)
compositionAt ss i = maybe (throwError OutOfBounds) pure (toList ss `atMay` i)

-- | Changes a focus with respect to a composition (timeline,
-- sequence, parallel, or track).
class ModifyFocus (t :: * -> *) where
  modifyFocus ::
       (ft ~ ToFocusType t)
    => t a
    -> FocusCommand
    -> Focus ft
    -> Either FocusError (Focus ft)

instance ModifyFocus Timeline where
  modifyFocus s e f = case (s, e, f) of
    -- Up
    (Timeline{}, FocusUp, SequenceFocus _ Nothing) -> throwError (CannotMove FocusUp)
    (Timeline seqs, FocusUp, SequenceFocus idx (Just parallelFocus)) -> do
      sequence' <- seqs `compositionAt` idx
      case modifyFocus sequence' FocusUp parallelFocus of
        Left  (CannotMove FocusUp) -> pure (SequenceFocus idx Nothing)
        Left  err                  -> throwError err
        Right f'                   -> pure (SequenceFocus idx (Just f'))

    -- Left
    (Timeline{}, FocusLeft, SequenceFocus idx Nothing)
      | idx > 0   -> pure (SequenceFocus (pred idx) Nothing)
      | otherwise -> throwError OutOfBounds

    -- Right
    (Timeline sub, FocusRight, SequenceFocus idx Nothing)
      | idx < (length sub - 1) -> pure (SequenceFocus (succ idx) Nothing)
      | otherwise              -> throwError OutOfBounds

    -- Down
    (Timeline seqs, FocusDown, SequenceFocus idx pf) -> do
      sequence'@(Sequence _ parallels') <- seqs `compositionAt` idx
      case pf of
        -- Down further within sequence.
        Just parallelFocus ->
          SequenceFocus idx
            .   Just
            <$> modifyFocus sequence' FocusDown parallelFocus
        -- Down from sequence into parallel.
        Nothing
          | null parallels' -> throwError (CannotMove FocusDown)
          | otherwise -> pure (SequenceFocus idx (Just (ParallelFocus 0 Nothing)))

    (Timeline seqs, _, SequenceFocus idx (Just parallelFocus)) -> do
      seq' <- seqs `compositionAt` idx
      SequenceFocus idx . Just <$> modifyFocus seq' e parallelFocus

instance ModifyFocus Sequence where
  modifyFocus s e f = case (s, e, f) of
    -- Up
    (Sequence{}, FocusUp, ParallelFocus _ Nothing) -> throwError (CannotMove FocusUp)
    -- In case we have a parent, we try to move up within the child, or
    -- fall back to focus this parent.
    (Sequence _ sub, FocusUp, ParallelFocus i (Just subFocus)) -> do
      sub' <- sub `compositionAt` i
      case modifyFocus sub' FocusUp subFocus of
        Left  (CannotMove FocusUp) -> pure (ParallelFocus i Nothing)
        Left  err                  -> throwError err
        Right f'                   -> pure (ParallelFocus i (Just f'))

    -- Right
    (Sequence _ sub, FocusRight, ParallelFocus idx Nothing)
      | idx < (length sub - 1) -> pure (ParallelFocus (succ idx) Nothing)
      | otherwise              -> throwError OutOfBounds

    -- Left
    (Sequence{}, FocusLeft, ParallelFocus idx Nothing)
      | idx > 0   -> pure (ParallelFocus (pred idx) Nothing)
      | otherwise -> throwError OutOfBounds

    -- Down further within a focused parallel.
    (Sequence _ parallels', FocusDown, ParallelFocus idx (Just trackFocus)) -> do
      parallel <- parallels' `compositionAt` idx
      ParallelFocus idx . Just <$> modifyFocus parallel FocusDown trackFocus

    (Sequence{}, FocusDown, ParallelFocus idx Nothing) ->
      pure (ParallelFocus idx (Just (TrackFocus Video Nothing)))

    -- Left or right further down within sequence.
    (Sequence _ pars, _, ParallelFocus idx (Just trackFocus)) -> do
      par <- pars `compositionAt` idx
      ParallelFocus idx . Just <$> modifyFocus par e trackFocus

instance ModifyFocus Parallel where
  modifyFocus s e f = case (s, e, f) of
    -- Up
    (Parallel{}, FocusUp, TrackFocus Video subFocus) ->
      case subFocus of
        Just _  -> pure (TrackFocus Video Nothing)
        Nothing -> throwError (CannotMove FocusUp)
    (Parallel{}, FocusUp, TrackFocus Audio subFocus) ->
      case subFocus of
        Just _  -> pure (TrackFocus Audio Nothing)
        Nothing -> pure (TrackFocus Video Nothing)

    -- We can move down from video to audio.
    (Parallel{}, FocusDown, TrackFocus Video subFocus) ->
      case subFocus of
        Just _  -> throwError (CannotMove FocusDown)
        Nothing -> pure (TrackFocus Audio Nothing)

    -- We cannot move down any further when focusing an audio track or clip.
    (Parallel{}, FocusDown, TrackFocus Audio _) -> throwError (CannotMove FocusDown)

    -- Left
    (Parallel{}, FocusLeft, TrackFocus _ Nothing) -> throwError (CannotMove FocusLeft)
    (Parallel{}, FocusLeft, TrackFocus type' (Just (ClipFocus 0))) -> pure (TrackFocus type' Nothing)
    (Parallel _ videoTrack' audioTrack', FocusLeft, TrackFocus type' (Just subFocus)) ->
      case type' of
        Video -> TrackFocus type' . Just <$> modifyFocus videoTrack' FocusLeft subFocus
        Audio -> TrackFocus type' . Just <$> modifyFocus audioTrack' FocusLeft subFocus

    -- Right
    (Parallel _ (VideoTrack _ vs) _, FocusRight, TrackFocus Video Nothing)
      | not (null vs) -> pure (TrackFocus Video (Just (ClipFocus 0)))
      | otherwise -> throwError (CannotMove FocusRight)
    (Parallel _ _ (AudioTrack _ as), FocusRight, TrackFocus Audio Nothing)
      | not (null as) -> pure (TrackFocus Audio (Just (ClipFocus 0)))
      | otherwise -> throwError (CannotMove FocusRight)
    (Parallel _ videoTrack' audioTrack', FocusRight, TrackFocus type' (Just subFocus)) ->
      case type' of
        Video -> TrackFocus type' . Just <$> modifyFocus videoTrack' FocusRight subFocus
        Audio -> TrackFocus type' . Just <$> modifyFocus audioTrack' FocusRight subFocus

instance ModifyFocus VideoTrack where
  modifyFocus s e f = case (s, e, f) of
    -- Left
    (VideoTrack _ parts', FocusLeft, ClipFocus idx)
      | idx > 0 && idx < length parts' -> pure (ClipFocus (pred idx))
      | otherwise -> throwError OutOfBounds
    -- Right
    (VideoTrack _ parts', FocusRight, ClipFocus idx)
      | idx >= 0 && idx < (length parts' - 1) -> pure (ClipFocus (succ idx))
      | otherwise -> throwError OutOfBounds
    (VideoTrack{}, cmd, _) -> throwError (CannotMove cmd)

instance ModifyFocus AudioTrack where
  modifyFocus s e f = case (s, e, f) of
    -- Left
    (AudioTrack _ parts', FocusLeft, ClipFocus idx)
      | idx > 0 && idx < length parts' -> pure (ClipFocus (pred idx))
      | otherwise -> throwError OutOfBounds
    -- Right
    (AudioTrack _ parts', FocusRight, ClipFocus idx)
      | idx >= 0 && idx < (length parts' - 1) -> pure (ClipFocus (succ idx))
      | otherwise -> throwError OutOfBounds
    (AudioTrack{}, cmd, _) -> throwError (CannotMove cmd)

data FocusedTraversal (f :: * -> *) a = FocusedTraversal
  { mapSequence   :: Sequence a -> f (Sequence a)
  , mapParallel   :: Parallel a -> f (Parallel a)
  , mapVideoTrack :: VideoTrack a -> f (VideoTrack a)
  , mapAudioTrack :: AudioTrack a -> f (AudioTrack a)
  , mapTrackPart  :: forall mt. SMediaType mt -> TrackPart mt a -> f (TrackPart mt a)
  }

class MapAtFocus (t :: * -> *) where
  mapAtFocus ::
       Applicative f
    => Focus (ToFocusType t)
    -> FocusedTraversal f a
    -> t a
    -> f (t a)

instance MapAtFocus Timeline where
  mapAtFocus focus t@FocusedTraversal{..} (Timeline sub) =
    case focus of
      SequenceFocus idx Nothing ->
        Timeline <$> mapAt idx mapSequence sub
      SequenceFocus idx (Just subFocus) ->
        Timeline <$> mapAt idx (mapAtFocus subFocus t) sub

instance MapAtFocus Sequence where
  mapAtFocus focus t@FocusedTraversal {..} (Sequence ann sub) =
    case focus of
      ParallelFocus idx Nothing -> Sequence ann <$> mapAt idx mapParallel sub
      ParallelFocus idx (Just subFocus) ->
        Sequence ann <$> mapAt idx (mapAtFocus subFocus t) sub

instance MapAtFocus Parallel where
  mapAtFocus (TrackFocus clipType (Just clipFocus)) traversal (Parallel ann videoTrack' audioTrack') =
    case clipType of
      Video ->
        Parallel ann <$> mapAtFocus clipFocus traversal videoTrack' <*> pure audioTrack'
      Audio ->
        Parallel ann <$> pure videoTrack' <*> mapAtFocus clipFocus traversal audioTrack'
  mapAtFocus (TrackFocus clipType Nothing) FocusedTraversal {..} (Parallel ann videoTrack' audioTrack') =
    case clipType of
      Video ->
        Parallel ann <$> mapVideoTrack videoTrack' <*> pure audioTrack'
      Audio ->
        Parallel ann <$> pure videoTrack' <*> mapAudioTrack audioTrack'

instance MapAtFocus VideoTrack where
  mapAtFocus (ClipFocus idx) FocusedTraversal {..} (VideoTrack ann videoParts) =
    VideoTrack ann <$> (videoParts & ix idx %%~ mapTrackPart SVideo)

instance MapAtFocus AudioTrack where
  mapAtFocus (ClipFocus idx) FocusedTraversal {..} (AudioTrack ann audioParts) =
    AudioTrack ann <$> (audioParts & ix idx %%~ mapTrackPart SAudio)

mapAt :: Applicative f => Int -> (a -> f a) -> NonEmpty a -> f (NonEmpty a)
mapAt idx f xs = toList xs & ix idx %%~ f <&> NonEmpty.fromList

type FocusedAt = SomeComposition

atFocus :: Focus 'SequenceFocusType -> Timeline a -> Maybe (FocusedAt a)
atFocus focus comp = execState (mapAtFocus focus traversal comp) Nothing
  where
    remember
      :: forall (t :: * -> *) a
       . (t a -> FocusedAt a)
      -> t a
      -> State (Maybe (FocusedAt a)) (t a)
    remember focused x = put (Just (focused x)) >> pure x
    traversal = FocusedTraversal
      { mapSequence        = remember SomeSequence
      , mapParallel        = remember SomeParallel
      , mapVideoTrack      = remember SomeVideoTrack
      , mapAudioTrack      = remember SomeAudioTrack
      , mapTrackPart = \case
        SVideo -> remember SomeVideoPart
        SAudio -> remember SomeAudioPart
      }

data FirstTrackPart a
  = FirstVideoPart (TrackPart 'Video a)
  | FirstAudioPart (TrackPart 'Audio a)

firstTrackPart
  :: Focus 'SequenceFocusType -> Timeline a -> Maybe (FirstTrackPart a)
firstTrackPart f s = atFocus f s >>= \case
  SomeSequence  s' -> firstInSequence s'
  SomeParallel  p  -> firstInParallel p
  SomeVideoTrack v  -> firstInVideoTrack v
  SomeAudioTrack a  -> firstInAudioTrack a
  SomeVideoPart v  -> Just (FirstVideoPart v)
  SomeAudioPart a  -> Just (FirstAudioPart a)
  where
    firstInSequence :: Sequence a -> Maybe (FirstTrackPart a)
    firstInSequence (Sequence _ ps) = firstInParallel (NonEmpty.head ps)
    firstInParallel :: Parallel a -> Maybe (FirstTrackPart a)
    firstInParallel = \case
      Parallel _ vt at' -> firstInVideoTrack vt <|> firstInAudioTrack at'
    firstInVideoTrack (VideoTrack _ vs) = FirstVideoPart <$> headMay vs
    firstInAudioTrack (AudioTrack _ as) = FirstAudioPart <$> headMay as

-- * Lenses


someCompositionAt
  :: Applicative f
  => Focus 'SequenceFocusType
  -> (Sequence a -> f (Sequence a))
  -> (Parallel a -> f (Parallel a))
  -> (VideoTrack a -> f (VideoTrack a))
  -> (AudioTrack a -> f (AudioTrack a))
  -> (VideoPart a -> f (VideoPart a))
  -> (AudioPart a -> f (AudioPart a))
  -> Timeline a
  -> f (Timeline a)
someCompositionAt focus sl pl vtl atl vpl apl = mapAtFocus
  focus
  FocusedTraversal
    { mapSequence  = sl
    , mapParallel  = pl
    , mapVideoTrack  = vtl
    , mapAudioTrack  = atl
    , mapTrackPart = \case
                       SVideo -> vpl
                       SAudio -> apl
    }

class CompositionTraversal parent child where
  focusing
    :: forall a
     . Focus (ToFocusType parent)
    -> Traversal' (parent a) (child a)

instance CompositionTraversal Timeline Sequence where
  focusing focus f (Timeline tl) = case focus of
    SequenceFocus idx Nothing     -> tl & ix idx %%~ f & fmap Timeline
    SequenceFocus _ (Just _Focus) -> pure (Timeline tl)

instance CompositionTraversal Timeline Parallel where
  focusing focus f (Timeline tl) = case focus of
    SequenceFocus _ Nothing -> pure (Timeline tl)
    SequenceFocus idx (Just subFocus) ->
      tl & ix idx . focusing subFocus %%~ f & fmap Timeline

instance CompositionTraversal Timeline VideoPart where
  focusing focus f (Timeline tl) = case focus of
    SequenceFocus _ Nothing -> pure (Timeline tl)
    SequenceFocus idx (Just subFocus) ->
      tl & ix idx . focusing subFocus %%~ f & fmap Timeline

instance CompositionTraversal Timeline AudioPart where
  focusing focus f (Timeline tl) = case focus of
    SequenceFocus _ Nothing -> pure (Timeline tl)
    SequenceFocus idx (Just subFocus) ->
      tl & ix idx . focusing subFocus %%~ f & fmap Timeline

instance CompositionTraversal Sequence Parallel where
  focusing focus f (Sequence ann ps) = case focus of
    ParallelFocus idx Nothing     -> ps & ix idx %%~ f & fmap (Sequence ann)
    ParallelFocus _ (Just _Focus) -> pure (Sequence ann ps)

instance CompositionTraversal Sequence VideoPart where
  focusing focus f (Sequence ann ps) = case focus of
    ParallelFocus _ Nothing -> pure (Sequence ann ps)
    ParallelFocus idx (Just subFocus) ->
      ps & ix idx . focusing subFocus %%~ f & fmap (Sequence ann)

instance CompositionTraversal Sequence AudioPart where
  focusing focus f (Sequence ann ps) = case focus of
    ParallelFocus _ Nothing -> pure (Sequence ann ps)
    ParallelFocus idx (Just subFocus) ->
      ps & ix idx . focusing subFocus %%~ f & fmap (Sequence ann)

instance CompositionTraversal Parallel VideoPart where
  focusing (TrackFocus _ Nothing) _ p = pure p
  focusing (TrackFocus clipType (Just clipFocus)) f (Parallel ann videoTrack' audioTrack') =
    case clipType of
      Video ->
        videoTrack'
        & focusing clipFocus %%~ f
        & fmap (\vs -> Parallel ann vs audioTrack')
      Audio -> pure (Parallel ann videoTrack' audioTrack')

instance CompositionTraversal Parallel AudioPart where
  focusing (TrackFocus _ Nothing) _ p = pure p
  focusing (TrackFocus clipType (Just clipFocus)) f (Parallel ann videoTrack' audioTrack') =
    case clipType of
      Video -> pure (Parallel ann videoTrack' audioTrack')
      Audio ->
        audioTrack'
        & focusing clipFocus %%~ f
        & fmap (Parallel ann videoTrack')

instance CompositionTraversal VideoTrack VideoPart where
  focusing (ClipFocus idx) f (VideoTrack ann parts') =
    parts'
    & ix idx %%~ f
    & fmap (VideoTrack ann)

instance CompositionTraversal AudioTrack AudioPart where
  focusing (ClipFocus idx) f (AudioTrack ann parts') =
    parts'
    & ix idx %%~ f
    & fmap (AudioTrack ann)
