{-# OPTIONS_GHC -fno-warn-unticked-promoted-constructors #-}

{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE StandaloneDeriving    #-}
{-# LANGUAGE TypeFamilies          #-}
module Komposition.UndoRedo
  ( Runnable(..)
  , History
  , init
  , current
  , size
  , numUndos
  , numRedos
  , runAndRecord
  , undo
  , redo
  )
where

import           Komposition.Prelude

import           Control.Lens        hiding (uncons)

class Runnable action state ret f where
  run :: action -> state -> f ((action, state), ret)

data History action state = History
  { _current :: state
  , toUndo   :: [action]
  , toRedo   :: [action]
  } deriving (Generic)

current :: Getter (History action state) state
current f (History state' toUndo toRedo) =
  f state' <&> \state'' -> History state'' toUndo toRedo

deriving instance
  (Eq state, Eq action, Eq action)
  => Eq (History action state)

deriving instance
  (Show state, Show action, Show action)
  => Show (History action state)

init :: state -> History action state
init current' = History current' mempty mempty

-- TODO: track size in structure instead of counting lists
size :: History action state -> Int
size History { toUndo, toRedo } = length toUndo + length toRedo

numUndos :: History action state -> Int
numUndos History { toUndo } = length toUndo

numRedos :: History action state -> Int
numRedos History { toRedo } = length toRedo

runAndRecord
  :: (Functor f, Runnable action state ret f)
  => action
  -> History action state
  -> f (History action state, ret)
runAndRecord action history =
  let f (inverted, state') = history
        { _current = state'
        , toUndo  = inverted : toUndo history
        , toRedo  = mempty -- clear the redo stack when we "branch" the
                    -- history
        }
  in run action (_current history) <&> (_1 %~ f)

undo
  :: (Functor f, Runnable action state ret f)
  => History action state
  -> Maybe (f (History action state, ret))
undo history = do
  (u, us) <- uncons (toUndo history)
  let f (inverted, state') = history
        { _current = state'
        , toUndo  = us
        , toRedo  = inverted : toRedo history
        }
  pure (run u (_current history) <&> (_1 %~ f))

redo
  :: (Functor f, Runnable action state ret f)
  => History action state
  -> Maybe (f (History action state, ret))
redo history = do
  (r, rs) <- uncons (toRedo history)
  let f (inverted, state') = history
        { _current = state'
        , toRedo  = rs
        , toUndo  = inverted : toUndo history
        }
  pure (run r (_current history) <&> (_1 %~ f))
