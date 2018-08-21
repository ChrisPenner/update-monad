{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}

module Lib where

import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Writer

class (Monad f, Monad g) =>
      Hallucinate f g
  where
  into :: (f a -> g a)

bracket :: Hallucinate f g => g a -> (a -> f b) -> (b -> g c) -> g c
bracket setup action teardown = do
  a <- setup
  b <- into $ action a
  teardown b

uninterruptibleMask :: Hallucinate m m => ((forall a. m a -> m a) -> m b) -> m b
uninterruptibleMask f = f into

-- data ExitCase b =
--   ExitCase b
-- gBracket ::
--      forall g a b c m. Hallucinate (ReaderT (ExitCase b) g) g
--   => g a
--   -> (a -> ExitCase b -> g c)
--   -> (a -> g b)
--   -> g (b, c)
-- gBracket setup handle action = bracket setup thing
--   where
--     thing :: a -> ReaderT (ExitCase b) m c
--     thing = undefined
instance Hallucinate (State a) (State (a, b)) where
  into sa = do
    (a, b) <- get
    let (x, a') = runState sa a
    put (a', b)
    return x

inc :: State Int ()
inc = modify (+ 1)

inc2 :: State (Int, String) ()
inc2 = do
  into inc
  modify (fmap (++ "hi"))

instance Hallucinate IO IO where
  into = id

instance Hallucinate (Reader s) (State s) where
  into r = get >>= pure . (runReader r)

instance (Hallucinate m (State s)) => Hallucinate (ReaderT s m) (State s) where
  into r = get >>= into . runReaderT r

instance Monoid s => Hallucinate (Writer s) (State s) where
  into w = do
    let (a, s) = runWriter w
    modify (`mappend` s)
    pure a

bracketIO :: IO a -> (a -> IO b) -> (a -> IO c) -> IO c
bracketIO setup action teardown =
  bracket setup (\a -> action a *> pure a) teardown
