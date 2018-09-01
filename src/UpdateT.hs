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

class ApplyAction p s where
  applyAction :: p -> s -> s

class (ApplyAction p s, Monad m) =>
      MonadUpdate m s p
  | m -> s
  , m -> p
  where
  putAction :: p -> m ()
  getState :: m s

type Update s p a = UpdateT s p Identity a

data UpdateT s p m a = UpdateT
  { runUpdateT :: (s -> m (p, a))
  } deriving (Functor)

instance (ApplyAction p s, Monad m, Monoid p) =>
         Applicative (UpdateT s p m) where
  pure a = UpdateT . const $ pure (mempty, a)
  UpdateT u <*> UpdateT t =
    UpdateT $ \s -> do
      (p, f) <- u s
      (p', a) <- t (applyAction p s)
      return (p' <> p, f a)

instance (ApplyAction p s, Monad m, Monoid p) => Monad (UpdateT s p m) where
  UpdateT u >>= f =
    UpdateT $ \s -> do
      (p, a) <- u s
      let UpdateT fs = f a
      (p', a) <- fs (applyAction p s)
      return (p <> p', a)

instance (Monad m, ApplyAction p s, Monoid p) =>
         MonadUpdate (UpdateT s p m) s p where
  putAction p = UpdateT $ \_ -> pure (p, ())
  getState = UpdateT $ \s -> pure (mempty, s)

instance ApplyAction (Endo s) s where
  applyAction (Endo f) = f

-- getState :: (Monoid p, Applicative m) => UpdateT s p m s
instance (Monad m) => MonadState s (UpdateT s (Endo s) m) where
  get = UpdateT $ \s -> pure (mempty, s)
  put s = UpdateT $ \_ -> pure (Endo $ const s, ())

instance (MonadIO m, Monoid p, ApplyAction p s) => MonadIO (UpdateT s p m) where
  liftIO m = UpdateT $ \_ -> liftIO m >>= \x -> return (mempty, x)

evalUpdateT ::
     (Monoid p, ApplyAction p s, Monad m) => UpdateT s p m a -> s -> m a
evalUpdateT u s = snd <$> runUpdateT u s

execUpdateT ::
     (Monoid p, ApplyAction p s, Monad m) => UpdateT s p m a -> s -> m s
execUpdateT u s = snd <$> runUpdateT (u *> getState) s

collectUpdateT ::
     (Monoid p, ApplyAction p s, Monad m) => UpdateT s p m a -> s -> m p
collectUpdateT u s = fst <$> runUpdateT u s

auditUpdateT ::
     (Monad m, Monoid p, ApplyAction p s) => UpdateT s p m a -> s -> m (s, p, a)
auditUpdateT u s = do
  (p, (a, s)) <- runUpdateT ((,) <$> u <*> getState) s
  return (s, p, a)

evalUpdate :: (Monoid p, ApplyAction p s) => Update s p a -> s -> a
evalUpdate u s = snd $ runUpdate u s

execUpdate :: (Monoid p, ApplyAction p s) => Update s p a -> s -> s
execUpdate u s = snd $ runUpdate (u *> getState) s

collectUpdate :: (Monoid p, ApplyAction p s) => Update s p a -> s -> p
collectUpdate u s = fst $ runUpdate u s

runUpdate :: (Monoid p, ApplyAction p s) => Update s p a -> s -> (p, a)
runUpdate u s = runIdentity $ runUpdateT u s

auditUpdate :: (Monoid p, ApplyAction p s) => Update s p a -> s -> (s, p, a)
auditUpdate u s = runIdentity $ auditUpdateT u s
