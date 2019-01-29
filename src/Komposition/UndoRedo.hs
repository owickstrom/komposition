{-# OPTIONS_GHC -fno-warn-unticked-promoted-constructors #-}

{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE TupleSections         #-}
{-# LANGUAGE TypeFamilies          #-}
module Komposition.UndoRedo where

import           Komposition.Prelude

import           Komposition.Composition
import qualified Komposition.Composition.Insert as Insert
import           Komposition.Focus

class Runnable action subject m where
  run :: action -> subject -> m subject

class Invertible action where
  invert :: action -> action

data Direction = Forward | Backward

newtype Directed (dir :: Direction) action =
  Directed { unDirected :: action}

invertDirected :: Directed dir action -> Directed (Flip dir) action
invertDirected (Directed action) = Directed action

type family Flip (d :: Direction) where
  Flip Forward = Backward
  Flip Backward = Forward

data History action = History
  { toUndo :: [Directed Backward action]
  , toRedo :: [Directed Forward action]
  }

init :: History action
init = History mempty mempty

-- TODO: track size in structure instead of counting lists
size :: History action -> Int
size History { toUndo, toRedo } = length toUndo + length toRedo

numUndos :: History action -> Int
numUndos History { toUndo } = length toUndo

numRedos :: History action -> Int
numRedos History { toRedo } = length toRedo

runAndRecord
  :: (Functor f, Invertible action, Runnable action subject f)
  => Directed Forward action
  -> subject
  -> History action
  -> f (History action, subject)
runAndRecord action subject history =
  (history { toUndo = invertDirected action : toUndo history }, )
    <$> run (unDirected action) subject

undo
  :: (Runnable action subject m, Invertible action)
  => History action
  -> subject
  -> Maybe (History action, m subject)
undo history subject = do
  (u, us) <- uncons (toUndo history)
  pure
    ( history { toUndo = us, toRedo = invertDirected u : toRedo history }
    , run (unDirected u) subject
    )

redo
  :: (Runnable action subject m, Invertible action)
  => History action
  -> subject
  -> Maybe (History action, m subject)
redo history subject = do
  (r, rs) <- uncons (toRedo history)
  pure
    ( history { toRedo = rs, toUndo = invertDirected r : toUndo history }
    , run (unDirected r) subject
    )

-- * Test

data UndoableTimelineRunnable dir
  = Delete (Focus SequenceFocusType) (SomeComposition ())
  | Insert (Focus SequenceFocusType) Insert.InsertPosition (SomeComposition ())
