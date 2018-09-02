{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE FunctionalDependencies #-}

module UpdateT where

import Control.Monad.State
import Data.Functor.Identity
import Data.Monoid

class (Monoid p) =>
      ApplyAction p s
  where
  applyAction :: p -> s -> s

class (ApplyAction p s, Monad m) =>
      MonadUpdate m p s
  | m -> s
  , m -> p
  where
  putAction :: p -> m ()
  getState :: m s

type Update p s a = UpdateT p s Identity a

data UpdateT p s m a = UpdateT
  { runUpdateT :: (s -> m (p, a))
  } deriving (Functor)

instance (ApplyAction p s, Monad m) => Applicative (UpdateT p s m) where
  pure a = UpdateT . const $ pure (mempty, a)
  UpdateT u <*> UpdateT t =
    UpdateT $ \s -> do
      (p, f) <- u s
      (p', a) <- t (applyAction p s)
      return (p' <> p, f a)

instance (ApplyAction p s, Monad m) => Monad (UpdateT p s m) where
  UpdateT u >>= f =
    UpdateT $ \s -> do
      (p, a) <- u s
      let UpdateT fs = f a
      (p', a) <- fs (applyAction p s)
      return (p <> p', a)

instance (Monad m, ApplyAction p s) => MonadUpdate (UpdateT p s m) p s where
  putAction p = UpdateT $ \_ -> pure (p, ())
  getState = UpdateT $ \s -> pure (mempty, s)

instance ApplyAction (Endo s) s where
  applyAction (Endo f) = f

instance (Monad m) => MonadState s (UpdateT (Endo s) s m) where
  get = UpdateT $ \s -> pure (mempty, s)
  put s = UpdateT $ \_ -> pure (Endo $ const s, ())

instance (MonadIO m, ApplyAction p s) => MonadIO (UpdateT p s m) where
  liftIO m = UpdateT $ \_ -> liftIO m >>= \x -> return (mempty, x)

evalUpdateT :: (ApplyAction p s, Monad m) => UpdateT p s m a -> s -> m a
evalUpdateT u s = snd <$> runUpdateT u s

execUpdateT :: (ApplyAction p s, Monad m) => UpdateT p s m a -> s -> m s
execUpdateT u s = snd <$> runUpdateT (u *> getState) s

collectUpdateT :: (ApplyAction p s, Monad m) => UpdateT p s m a -> s -> m p
collectUpdateT u s = fst <$> runUpdateT u s

auditUpdateT ::
     (Monad m, ApplyAction p s) => UpdateT p s m a -> s -> m (s, p, a)
auditUpdateT u s = do
  (p, (a, s)) <- runUpdateT ((,) <$> u <*> getState) s
  return (s, p, a)

evalUpdate :: (ApplyAction p s) => Update p s a -> s -> a
evalUpdate u s = snd $ runUpdate u s

execUpdate :: (ApplyAction p s) => Update p s a -> s -> s
execUpdate u s = snd $ runUpdate (u *> getState) s

collectUpdate :: (ApplyAction p s) => Update p s a -> s -> p
collectUpdate u s = fst $ runUpdate u s

runUpdate :: (ApplyAction p s) => Update p s a -> s -> (p, a)
runUpdate u s = runIdentity $ runUpdateT u s

auditUpdate :: (ApplyAction p s) => Update p s a -> s -> (s, p, a)
auditUpdate u s = runIdentity $ auditUpdateT u s
