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
