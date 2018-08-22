{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}

module UpdateState where

import FreeUpdate

import Control.Monad.State
import Data.Maybe
import Data.Monoid

instance (Monad m) => MonadState s (FreeUpdateT s (Last s) m) where
  get = currentState
  put s = action (pure $ Last (Just s))

type UStateT s m a = FreeUpdateT s (Last s) m a

uRunStateT :: (Monad m) => UStateT s m a -> s -> m (s, a)
uRunStateT m s = do
  evalUpdateT (addState m) next s
  where
    next s p = fromMaybe s $ getLast p
    addState u = do
      a <- u
      s <- currentState
      return (s, a)

execUpdateT :: (Monad m) => FreeUpdateT s p m a -> (s -> p -> s) -> s -> m s
execUpdateT u next s = snd <$> runUpdateT (u *> currentState) next s

testState :: UStateT Int IO ()
testState = do
  n1 <- get
  liftIO $ print n1
  modify (+ 1)
  n2 <- get
  liftIO $ print n2
  put 42
  get >>= liftIO . print
