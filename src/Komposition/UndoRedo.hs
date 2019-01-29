{-# OPTIONS_GHC -fno-warn-unticked-promoted-constructors #-}

{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns        #-}
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

invertDirected
  :: Invertible action => Directed dir action -> Directed (Flip dir) action
invertDirected (Directed action) = Directed (invert action)

type family Flip (d :: Direction) where
  Flip Forward = Backward
  Flip Backward = Forward

data History action state = History
  { current :: state
  , toUndo  :: [Directed Backward action]
  , toRedo  :: [Directed Forward action]
  }

init :: state -> History action state
init current = History current mempty mempty

-- TODO: track size in structure instead of counting lists
size :: History action state -> Int
size History { toUndo, toRedo } = length toUndo + length toRedo

numUndos :: History action state -> Int
numUndos History { toUndo } = length toUndo

numRedos :: History action state -> Int
numRedos History { toRedo } = length toRedo

runAndRecord
  :: (Functor f, Invertible action, Runnable action state f)
  => Directed Forward action
  -> History action state
  -> f (History action state)
runAndRecord action history =
  run (unDirected action) (current history) <&> \c ->
    history { current = c, toUndo = invertDirected action : toUndo history }

undo
  :: (Functor f, Runnable action state f, Invertible action)
  => History action state
  -> Maybe (f (History action state))
undo history = do
  (u, us) <- uncons (toUndo history)
  pure $ run (unDirected u) (current history) <&> \c -> history
    { current = c
    , toUndo  = us
    , toRedo  = invertDirected u : toRedo history
    }

redo
  :: (Functor f, Runnable action state f, Invertible action)
  => History action state
  -> Maybe (f (History action state))
redo history = do
  (r, rs) <- uncons (toRedo history)
  pure $ run (unDirected r) (current history) <&> \c -> history
    { current = c
    , toRedo  = rs
    , toUndo  = invertDirected r : toUndo history
    }

-- * Test

data UndoableTimelineRunnable dir
  = Delete (Focus SequenceFocusType) (SomeComposition ())
  | Insert (Focus SequenceFocusType) Insert.InsertPosition (SomeComposition ())
