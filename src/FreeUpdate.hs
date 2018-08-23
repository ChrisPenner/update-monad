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
  { runUpdateT :: (s -> p -> s) -> s -> m ([p], a)
  } deriving (Functor)

instance (Monad m) => Applicative (FreeUpdateT s p m) where
  pure a = FreeUpdateT $ \_ _ -> pure (mempty, a)
  (<*>) = ap

instance (Monad m) => Monad (FreeUpdateT s p m) where
  FreeUpdateT u >>= f =
    FreeUpdateT $ \next s -> do
      (ps, a) <- u next s
      let FreeUpdateT fs = f a
      (ps', b) <- fs next (foldl' next s ps)
      return (ps <> ps', b)

instance (MonadIO m) => MonadIO (FreeUpdateT s p m) where
  liftIO io = FreeUpdateT $ \_ _ -> (mempty, ) <$> liftIO io

action :: Applicative m => p -> FreeUpdateT s p m ()
action p = FreeUpdateT $ \next _ -> pure ([p], ())

currentState :: Applicative m => FreeUpdateT s p m s
currentState = FreeUpdateT $ \n s -> pure (mempty, s)

evalUpdateT :: (Functor m) => FreeUpdateT s p m a -> (s -> p -> s) -> s -> m a
evalUpdateT u next s = snd <$> runUpdateT u next s

execUpdateT :: (Monad m) => FreeUpdateT s p m a -> (s -> p -> s) -> s -> m s
execUpdateT u next s = snd <$> runUpdateT (u *> currentState) next s

collectUpdateT ::
     (Functor m) => FreeUpdateT s p m a -> (s -> p -> s) -> s -> m [p]
collectUpdateT u next s = fst <$> runUpdateT u next s

type FreeUpdate s p a = FreeUpdateT s p Identity a

evalUpdate :: FreeUpdate s p a -> (s -> p -> s) -> s -> a
evalUpdate u next s = snd $ runUpdate u next s

execUpdate :: FreeUpdate s p a -> (s -> p -> s) -> s -> s
execUpdate u next s = snd $ runUpdate (u *> currentState) next s

collectUpdate :: FreeUpdate s p a -> (s -> p -> s) -> s -> [p]
collectUpdate u next s = fst $ runUpdate u next s

runUpdate :: FreeUpdate s p a -> (s -> p -> s) -> s -> ([p], a)
runUpdate u next s = runIdentity $ runUpdateT u next s
