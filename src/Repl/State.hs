{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}
module Repl.State where

import Common
import Core.Env
import Control.Lens

data ReplState = ReplState
  { _tyEnv :: TyEnv
  , _consEnv :: ConstructorEnv}

makeLenses ''ReplState

initialState :: ReplState
initialState = ReplState
  { _tyEnv = mempty
  , _consEnv = mempty}

getTyAndConsEnv :: MonadState ReplState m => m (TyEnv, ConstructorEnv)
getTyAndConsEnv = (,) <$> use tyEnv <*> use consEnv