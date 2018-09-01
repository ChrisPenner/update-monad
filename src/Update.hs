{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE FunctionalDependencies #-}

module Update where

import Control.Monad.State
import Data.Functor.Identity
import Data.Monoid

class ApplyAction s p where
  act :: s -> p -> s

class (ApplyAction s p) =>
      MonadUpdate m s p
  | m -> s
  , m -> p
  where
  action :: p -> m ()
  getState :: m s

data UpdateT s p m a = UpdateT
  { runUpdateT :: (s -> m (p, a))
  } deriving (Functor)

type Update s p a = UpdateT s p Identity a

instance (ApplyAction s p, Monad m, Monoid p) =>
         Applicative (UpdateT s p m) where
  pure a = UpdateT . const $ pure (mempty, a)
  UpdateT u <*> UpdateT t =
    UpdateT $ \s -> do
      (p, f) <- u s
      (p', a) <- t (act s p)
      return (p' <> p, f a)

instance (ApplyAction s p, Monad m, Monoid p) => Monad (UpdateT s p m) where
  UpdateT u >>= f =
    UpdateT $ \s -> do
      (p, a) <- u s
      let UpdateT fs = f a
      (p', a) <- fs (act s p)
      return (p <> p', a)

instance (Monad m, ApplyAction s p, Monoid p) =>
         MonadUpdate (UpdateT s p m) s p where
  action p = UpdateT $ \_ -> pure (p, ())
  getState = UpdateT $ \s -> pure (mempty, s)

instance ApplyAction s (Endo s) where
  act s (Endo f) = f s

-- getState :: (Monoid p, Applicative m) => UpdateT s p m s
instance (Monad m) => MonadState s (UpdateT s (Endo s) m) where
  get = UpdateT $ \s -> pure (mempty, s)
  put s = UpdateT $ \_ -> pure (Endo $ const s, ())

instance (MonadIO m, Monoid p, ApplyAction s p) => MonadIO (UpdateT s p m) where
  liftIO m = UpdateT $ \_ -> liftIO m >>= \x -> return (mempty, x)

evalUpdateT ::
     (Monoid p, ApplyAction s p, Monad m) => UpdateT s p m a -> s -> m a
evalUpdateT u s = snd <$> runUpdateT u s

execUpdateT ::
     (Monoid p, ApplyAction s p, Monad m) => UpdateT s p m a -> s -> m s
execUpdateT u s = snd <$> runUpdateT (u *> getState) s

collectUpdateT ::
     (Monoid p, ApplyAction s p, Monad m) => UpdateT s p m a -> s -> m p
collectUpdateT u s = fst <$> runUpdateT u s

auditUpdateT ::
     (Monad m, Monoid p, ApplyAction s p) => UpdateT s p m a -> s -> m (s, p, a)
auditUpdateT u s = do
  (p, (a, s)) <- runUpdateT ((,) <$> u <*> getState) s
  return (s, p, a)

evalUpdate :: (Monoid p, ApplyAction s p) => Update s p a -> s -> a
evalUpdate u s = snd $ runUpdate u s

execUpdate :: (Monoid p, ApplyAction s p) => Update s p a -> s -> s
execUpdate u s = snd $ runUpdate (u *> getState) s

collectUpdate :: (Monoid p, ApplyAction s p) => Update s p a -> s -> p
collectUpdate u s = fst $ runUpdate u s

runUpdate :: (Monoid p, ApplyAction s p) => Update s p a -> s -> (p, a)
runUpdate u s = runIdentity $ runUpdateT u s

auditUpdate :: (Monoid p, ApplyAction s p) => Update s p a -> s -> (s, p, a)
auditUpdate u s = runIdentity $ auditUpdateT u s
