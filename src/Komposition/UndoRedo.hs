{-# OPTIONS_GHC -fno-warn-unticked-promoted-constructors #-}

{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE StandaloneDeriving    #-}
{-# LANGUAGE TemplateHaskell       #-}
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

class Runnable action subject f where
  run :: action -> subject -> f (action, subject)

data History action state = History
  { _current :: state
  , toUndo   :: [action]
  , toRedo   :: [action]
  }

deriving instance
  (Eq state, Eq action, Eq action)
  => Eq (History action state)

deriving instance
  (Show state, Show action, Show action)
  => Show (History action state)

makeLenses ''History

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
  :: (Functor f, Runnable action state f)
  => action
  -> History action state
  -> f (History action state)
runAndRecord action history =
  let f (inverted, state') = history
        { _current = state'
        , toUndo  = inverted : toUndo history
        , toRedo  = mempty -- clear the redo stack when we "branch" the
                    -- history
        }
  in f <$> run action (_current history)

undo
  :: (Functor f, Runnable action state f)
  => History action state
  -> Maybe (f (History action state))
undo history = do
  (u, us) <- uncons (toUndo history)
  let f (inverted, state') = history
        { _current = state'
        , toUndo  = us
        , toRedo  = inverted : toRedo history
        }
  pure (f <$> run u (_current history))

redo
  :: (Functor f, Runnable action state f)
  => History action state
  -> Maybe (f (History action state))
redo history = do
  (r, rs) <- uncons (toRedo history)
  let f (inverted, state') = history
        { _current = state'
        , toRedo  = rs
        , toUndo  = inverted : toUndo history
        }
  pure (f <$> run r (_current history))
