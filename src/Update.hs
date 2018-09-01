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

class ApplyAction p s where
  applyAction :: p -> s -> s

class (ApplyAction p s, Monad m) =>
      MonadUpdate m s p
  | m -> s
  , m -> p
  where
  putAction :: p -> m ()
  getState :: m s

data Update s p a = Update
  { runUpdate :: (s -> (p, a))
  } deriving (Functor)

instance (ApplyAction p s, Monoid p) => Applicative (Update s p) where
  pure a = Update . const $ (mempty, a)
  Update u <*> Update t =
    Update $ \s ->
      let (p, f) = u s
          (p', a) = t (applyAction p s)
       in (p' <> p, f a)

instance (ApplyAction p s, Monoid p) => Monad (Update s p) where
  Update u >>= f =
    Update $ \s ->
      let (p, a) = u s
          Update fs = f a
          (p', a') = fs (applyAction p s)
       in (p <> p', a')

instance (ApplyAction p s, Monoid p) => MonadUpdate (Update s p) s p where
  putAction p = Update $ \_ -> (p, ())
  getState = Update $ \s -> (mempty, s)

instance ApplyAction (Endo s) s where
  applyAction (Endo f) = f

-- getState :: (Monoid p, Applicative m) => Update s p s
instance MonadState s (Update s (Endo s)) where
  get = Update $ \s -> (mempty, s)
  put s = Update $ \_ -> (Endo $ const s, ())

evalUpdate :: (Monoid p, ApplyAction p s) => Update s p a -> s -> a
evalUpdate u s = snd $ runUpdate u s

execUpdate :: (Monoid p, ApplyAction p s) => Update s p a -> s -> s
execUpdate u s = snd $ runUpdate (u *> getState) s

collectUpdate :: (Monoid p, ApplyAction p s) => Update s p a -> s -> p
collectUpdate u s = fst $ runUpdate u s

auditUpdate :: (Monoid p, ApplyAction p s) => Update s p a -> s -> (s, p, a)
auditUpdate u s =
  let (p, (a, s)) = runUpdate ((,) <$> u <*> getState) s
   in (s, p, a)
