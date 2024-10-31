{-# LANGUAGE GeneralisedNewtypeDeriving #-}
module Control.Monad.Check (Check(..)) where

import Common

type ErrorMsg = String

newtype Check a = Check {runCheck :: Either ErrorMsg a}
  deriving (Functor, Applicative, Monad)

instance MonadFail Check where
  fail = Check . Left