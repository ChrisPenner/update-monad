{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module State where

import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Writer

newtype StateReader r a =
  StateReader (State r a)
  deriving (Functor, Applicative, Monad)

instance MonadReader r (StateReader r)
  -- ask just returns the state
                                where
  ask = StateReader get
  local mod (StateReader m) =
    StateReader $
      -- We want to keep track the initial state so we can set it back after we
      -- temporarily edit our state.
     do
      before <- get
      modify mod
      a <- m
      put before
      return a

newtype StateWriter w a =
  StateWriter (State w a)
  deriving (Functor, Applicative, Monad)

instance Monoid w => MonadWriter w (StateWriter w)
  -- tell just mappends onto our state
                                       where
  tell m = StateWriter (modify (`mappend` m))
  -- listen collects some new state which we return, then we apply it to our
  -- running tally.
  listen (StateWriter m) =
    let (a, s) = runState m mempty
     in tell s >> return (a, s)
  -- pass runs some action in our monad which results in a function which
  -- we allow to modify the new state before we append it to the state we've
  -- already collected
  pass (StateWriter m) =
    let ((a, f), s) = runState m mempty
     in tell (f s) >> return a
