{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TupleSections #-}
module Control.Monad.RS(RST(..), evalRST, execRST) where

import Common

newtype RST r s m a = RST {runRST :: r -> s -> m (a,s)}

instance Monad m => Functor (RST r s m) where
  fmap f (RST run) = RST $ \r s -> do
    (x, s') <- run r s
    return (f x, s')
instance Monad m => Applicative (RST r s m) where
  pure x = RST $ \_ s -> return (x, s)
  RST runF <*> RST runX = RST $ \r s -> do
    (f, sF) <- runF r s
    (x, sX) <- runX r sF
    return (f x, sX)
instance Monad m => Monad (RST r s m) where
  return = pure
  RST run >>= f = RST $ \r s -> do
    (x, s') <- run r s
    let RST runF = f x
    runF r s'
instance MonadTrans (RST r s) where
  lift m = RST $ \_ s -> (, s) <$> m
instance Monad m => MonadReader r (RST r s m) where
  ask = RST $ curry return
  local f (RST run) = RST $ \r s -> run (f r) s
instance Monad m => MonadState s (RST r s m) where
  get = RST $ \_ s -> return (s, s)
  put s = RST $ \_ _ -> return ((),s)
instance MonadFail m => MonadFail (RST r s m) where
  fail msg = lift (fail msg)

evalRST :: Monad m => RST r s m a -> r -> s -> m a
evalRST rst r s = fst <$> runRST rst r s

execRST :: Monad m => RST r s m a -> r -> s -> m s
execRST rst r s = snd <$> runRST rst r s