{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TupleSections #-}

module Update where

import Control.Monad.State
import Data.Monoid

class Absorb s p where
  act :: s -> p -> s

data UpdateT s p m a =
  UpdateT (s -> m (p, a))
  deriving (Functor)

instance (Absorb s p, Monad m, Monoid p) => Applicative (UpdateT s p m) where
  pure a = UpdateT . const $ pure (mempty, a)
  UpdateT u <*> UpdateT t =
    UpdateT $ \s -> do
      (p, f) <- u s
      (p', a) <- t (act s p)
      return (p <> p', f a)

instance (Absorb s p, Monad m, Monoid p) => Monad (UpdateT s p m) where
  UpdateT u >>= f =
    UpdateT $ \s -> do
      (p, a) <- u s
      let UpdateT fs = f a
      fs (act s p)

data FreeUpdateT s p m a = FreeUpdateT
  { runFreeUpdateT :: (s -> [p] -> s) -> s -> m ([p], a)
  } deriving (Functor)

instance (Monad m) => Applicative (FreeUpdateT s p m) where
  pure a = FreeUpdateT $ \_ _ -> pure (mempty, a)
  FreeUpdateT u <*> FreeUpdateT t =
    FreeUpdateT $ \next s -> do
      (p, f) <- u next s
      (p', a) <- t next (next s p)
      return (p ++ p', f a)

instance (Monad m) => Monad (FreeUpdateT s p m) where
  FreeUpdateT u >>= f =
    FreeUpdateT $ \next s -> do
      (p, a) <- u next s
      let FreeUpdateT fs = f a
      (p', b) <- fs next (next s p)
      return (p <> p', b)

runFreeUpdate :: (s -> [p] -> s) -> FreeUpdateT s p m a -> UpdateT s [p] m a
runFreeUpdate f (FreeUpdateT u) = UpdateT (u f)

instance Absorb Int (Sum Int) where
  act s (Sum i) = s + i

instance (MonadIO m) => MonadIO (FreeUpdateT s p m) where
  liftIO io = FreeUpdateT $ \_ _ -> (mempty, ) <$> liftIO io

action :: Functor m => m p -> FreeUpdateT s p m ()
action m = FreeUpdateT $ \next _ -> (\x -> ([x], ())) <$> m

addLength :: FreeUpdateT s String IO ()
addLength = action getLine

prog :: FreeUpdateT s String IO ()
prog = do
  addLength
  addLength
  addLength

get' :: Monad m => FreeUpdateT s p m s
get' = FreeUpdateT $ \n s -> pure (mempty, s)

runProgReal :: FreeUpdateT Int String IO () -> IO ()
runProgReal u = do
  cnt <-
    snd <$>
    runFreeUpdateT
      (u *> get')
      (\i s -> i + (getSum . foldMap (Sum . length) $ s))
      0
  print cnt

instance Absorb [a] a where
  act s a = a : s

instance Absorb [a] [a] where
  act s a = a ++ s

runProgFake :: FreeUpdateT [String] String IO () -> IO ()
runProgFake u = do
  cnt <- snd <$> (runFreeUpdateT (u *> get')) (++) []
  print cnt
