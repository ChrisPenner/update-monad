{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}

module UpdateState where

import FreeUpdate

import Control.Monad.State
import Data.Maybe
import Data.Monoid

type UStateT s m a = FreeUpdateT s s m a

instance (Monad m) => MonadState s (FreeUpdateT s s m) where
  get = currentState
  put s = action s

uRunStateT :: (Monad m) => UStateT s m a -> s -> m (s, a)
uRunStateT m s = do
  evalUpdateT (addState m) next s
  where
    next _ p = p
    addState u = do
      a <- u
      s <- currentState
      return (s, a)

testState :: UStateT Int IO ()
testState = do
  n1 <- get
  liftIO $ print n1
  modify (+ 1)
  n2 <- get
  liftIO $ print n2
  put 42
  get >>= liftIO . print
