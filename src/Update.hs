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

data Update p s a = Update
  { runUpdate :: (s -> (p, a))
  } deriving (Functor)

instance (ApplyAction p s) => Applicative (Update p s) where
  pure a = Update $ \_ -> (mempty, a)
  Update u <*> Update t =
    Update $ \s
      -- Run the first 'Update' with the initial state 
      -- and get the monoidal action and the function out
     ->
      let (p, f) = u s
      -- Run the second 'Update' with a state which has been altered by
      -- the first action to get the 'a' and another action
          (p', a) = t (applyAction p s)
      -- Combine the actions together and run the function
       in (p' <> p, f a)

instance (ApplyAction p s) => Monad (Update p s) where
  Update u >>= f =
    Update $ \s
      -- Run the first 'Update' with the initial state 
      -- and get the monoidal action and the function out
     ->
      let (p, a) = u s
      -- Run the given function over our resulting value to get our next Update
          Update t = f a
      -- Run our new 'Update' over the altered state
          (p', a') = t (applyAction p s)
      -- Combine the actions together and return the result
       in (p <> p', a')

instance (ApplyAction p s) => MonadUpdate (Update p s) p s where
  putAction p = Update $ \_ -> (p, ())
  getState = Update $ \s -> (mempty, s)

instance ApplyAction (Endo s) s where
  applyAction (Endo f) = f

instance MonadState s (Update (Endo s) s) where
  get = Update $ \s -> (mempty, s)
  put s = Update $ \_ -> (Endo $ const s, ())

evalUpdate :: (ApplyAction p s) => Update p s a -> s -> a
evalUpdate u s = snd $ runUpdate u s

execUpdate :: (ApplyAction p s) => Update p s a -> s -> s
execUpdate u s = snd $ runUpdate (u *> getState) s

collectUpdate :: (ApplyAction p s) => Update p s a -> s -> p
collectUpdate u s = fst $ runUpdate u s

auditUpdate :: (ApplyAction p s) => Update p s a -> s -> (s, p, a)
auditUpdate u s = 
  let (p, (a, s')) = runUpdate ((,) <$> u <*> getState) s
  in (s', p, a)
