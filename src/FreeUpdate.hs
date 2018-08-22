{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TupleSections #-}

module FreeUpdate where

import Control.Monad.Reader
import Control.Monad.State
import Data.Foldable
import Data.Monoid

class Absorb s p where
  act :: s -> p -> s

data FreeUpdateT s p m a = FreeUpdateT
  { runFreeUpdateT :: (s -> p -> s) -> s -> m ([p], a)
  } deriving (Functor)

instance (Monad m) => Applicative (FreeUpdateT s p m) where
  pure a = FreeUpdateT $ \_ _ -> pure (mempty, a)
  FreeUpdateT u <*> FreeUpdateT t =
    FreeUpdateT $ \next s -> do
      (ps, f) <- u next s
      (ps', a) <- t next (foldl' next s ps)
      return (ps <> ps', f a)

instance (Monad m) => Monad (FreeUpdateT s p m) where
  FreeUpdateT u >>= f =
    FreeUpdateT $ \next s -> do
      (ps, a) <- u next s
      let FreeUpdateT fs = f a
      (ps', b) <- fs next (foldl' next s ps)
      return (ps <> ps', b)

instance Absorb Int (Sum Int) where
  act s (Sum i) = s + i

instance (MonadIO m) => MonadIO (FreeUpdateT s p m) where
  liftIO io = FreeUpdateT $ \_ _ -> (mempty, ) <$> liftIO io

action :: Functor m => m p -> FreeUpdateT s p m ()
action m = FreeUpdateT $ \next _ -> (\x -> ([x], ())) <$> m

currentState :: Applicative m => FreeUpdateT s p m s
currentState = FreeUpdateT $ \n s -> pure (mempty, s)

instance (Monad m) => MonadState s (FreeUpdateT s (Last s) m) where
  get = currentState
  put s = action (pure $ Last (Just s))

evalUpdateT :: (Functor m) => FreeUpdateT s p m a -> (s -> p -> s) -> s -> m a
evalUpdateT u next s = snd <$> runUpdateT u next s

execUpdateT :: (Monad m) => FreeUpdateT s p m a -> (s -> p -> s) -> s -> m s
execUpdateT u next s = snd <$> runUpdateT (u *> currentState) next s

collectUpdateT ::
     (Functor m) => FreeUpdateT s p m a -> (s -> p -> s) -> s -> m [p]
collectUpdateT u next s = fst <$> runUpdateT u next s

runUpdateT ::
     (Functor m) => FreeUpdateT s p m a -> (s -> p -> s) -> s -> m ([p], a)
runUpdateT (FreeUpdateT u) = u

addLength :: FreeUpdateT s String IO ()
addLength = action getLine

prog :: FreeUpdateT s String IO ()
prog = do
  addLength
  addLength
  addLength

runProgFake :: FreeUpdateT [String] String IO () -> IO ()
runProgFake u = do
  collection <- collectUpdateT u (flip (:)) []
  print collection

runProgReal :: FreeUpdateT Int String IO () -> IO ()
runProgReal u = do
  cnt <- execUpdateT u (\i s -> i + (length $ s)) 0
  print cnt
