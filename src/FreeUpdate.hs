{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TupleSections #-}

module FreeUpdate where

import Control.Monad.Identity
import Control.Monad.State
import Data.Foldable
import Data.Monoid

data FreeUpdateT s p m a = FreeUpdateT
  { runFreeUpdateT :: (p -> s -> s) -> s -> m ([p], a)
  } deriving (Functor)

instance (Monad m) => Applicative (FreeUpdateT s p m) where
  pure a = FreeUpdateT $ \_ _ -> pure (mempty, a)
  (<*>) = ap

instance (Monad m) => Monad (FreeUpdateT s p m) where
  FreeUpdateT u >>= f =
    FreeUpdateT $ \next s -> do
      (ps, a) <- u next s
      let FreeUpdateT fs = f a
      (ps', b) <- fs next (foldl' (flip next) s ps)
      return (ps <> ps', b)

instance (MonadIO m) => MonadIO (FreeUpdateT s p m) where
  liftIO io = FreeUpdateT $ \_ _ -> (mempty, ) <$> liftIO io

action :: Applicative m => p -> FreeUpdateT s p m ()
action p = FreeUpdateT $ \next _ -> pure ([p], ())

currentState :: Applicative m => FreeUpdateT s p m s
currentState = FreeUpdateT $ \n s -> pure (mempty, s)

evalFreeUpdateT ::
     (Functor m) => FreeUpdateT s p m a -> (p -> s -> s) -> s -> m a
evalFreeUpdateT u next s = snd <$> runFreeUpdateT u next s

execFreeUpdateT :: (Monad m) => FreeUpdateT s p m a -> (p -> s -> s) -> s -> m s
execFreeUpdateT u next s = snd <$> runFreeUpdateT (u *> currentState) next s

collectUpdateT ::
     (Functor m) => FreeUpdateT s p m a -> (p -> s -> s) -> s -> m [p]
collectUpdateT u next s = fst <$> runFreeUpdateT u next s

type FreeUpdate s p a = FreeUpdateT s p Identity a

evalFreeUpdate :: FreeUpdate s p a -> (p -> s -> s) -> s -> a
evalFreeUpdate u next s = snd $ runFreeUpdate u next s

execFreeUpdate :: FreeUpdate s p a -> (p -> s -> s) -> s -> s
execFreeUpdate u next s = snd $ runFreeUpdate (u *> currentState) next s

collectFreeUpdate :: FreeUpdate s p a -> (p -> s -> s) -> s -> [p]
collectFreeUpdate u next s = fst $ runFreeUpdate u next s

runFreeUpdate :: FreeUpdate s p a -> (p -> s -> s) -> s -> ([p], a)
runFreeUpdate u next s = runIdentity $ runFreeUpdateT u next s
