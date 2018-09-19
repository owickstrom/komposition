{-# LANGUAGE DataKinds  #-}
{-# LANGUAGE GADTs      #-}
{-# LANGUAGE LambdaCase #-}
-- | Annotate a composition with focus metadata.

module FastCut.Composition.Focused where

import           FastCut.Prelude

import           Data.List.NonEmpty  (NonEmpty (..))
import qualified Data.List.NonEmpty  as NonEmpty

import           FastCut.Composition
import           FastCut.Focus
import           FastCut.MediaType

data Focused
  = Focused
  | TransitivelyFocused
  | Blurred
  deriving (Eq, Show)

-- | Given a current focus, returns whether the other focus is focused
-- (the same as the first), transitively focused (a sub-path of the
-- current focus), or not focused at all.
focusedState
  :: Focus ft -- ^ Current focus
  -> Focus ft -- ^ A focus to check
  -> Focused
focusedState f1 f2 =
  case (f1, f2) of
    (SequenceFocus i1 mf1, SequenceFocus i2 mf2)
      | i1 == i2 -> subFocusState (mf1, mf2)
    (ParallelFocus i1 mf1, ParallelFocus i2 mf2)
      | i1 == i2 -> subFocusState (mf1, mf2)
    (ClipFocus mt1 i1, ClipFocus mt2 i2) ->
      if mt1 == mt2 && i1 == i2
      then Focused
      else Blurred
    _ -> Blurred
  where
    subFocusState =
      \case
        (Nothing, Nothing) -> Focused
        (Just _, Nothing) -> TransitivelyFocused
        (Just f1', Just f2') -> focusedState f1' f2'
        (Nothing, Just _) -> Blurred

numsFromZero :: (Enum n, Num n) => NonEmpty n
numsFromZero = 0 :| [1 ..]

withAllFoci :: Timeline a -> Timeline (Focus SequenceFocusType)
withAllFoci (Timeline sub) =
  Timeline
    (NonEmpty.zipWith (\i -> onSequence (SequenceFocus i)) numsFromZero sub)
  where
    onSequence ::
         (Maybe (Focus ParallelFocusType) -> Focus SequenceFocusType)
      -> Sequence a
      -> Sequence (Focus SequenceFocusType)
    onSequence wrap (Sequence _ pars) =
      Sequence
        (wrap Nothing)
        (NonEmpty.zipWith
           (\i -> onParallel (wrap . Just . ParallelFocus i))
           numsFromZero
           pars)
    onParallel ::
         (Maybe (Focus ClipFocusType) -> Focus SequenceFocusType)
      -> Parallel a
      -> Parallel (Focus SequenceFocusType)
    onParallel wrap (Parallel _ vs as) =
        Parallel
        (wrap Nothing)
        (zipWith (onCompositionPart . wrap . Just . ClipFocus Video) [0 ..] vs)
        (zipWith (onCompositionPart . wrap . Just . ClipFocus Audio) [0 ..] as)
    onCompositionPart focus = ($> focus)
