{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}

module SADT.Repl.State where

import Control.Lens
import SADT.Common
import SADT.Data.Env
import SADT.Data.Expr
import SADT.Data.Type
import SADT.Data.Val

data ReplState = ReplState
  { _valEnv :: ValEnv,
    _tyEnv :: TyEnv,
    _consEnv :: ConstructorEnv
  }

makeLenses ''ReplState

initialState :: ReplState
initialState =
  ReplState
    { _valEnv = mempty,
      _tyEnv = mempty,
      _consEnv = mempty
    }

getEnvs :: (MonadState ReplState m) => m (ValEnv, TyEnv, ConstructorEnv)
getEnvs = do
  vEnv <- use valEnv
  tEnv <- use tyEnv
  cEnv <- use consEnv
  return (vEnv, tEnv, cEnv)

getTyAndConsEnv :: (MonadState ReplState m) => m (TyEnv, ConstructorEnv)
getTyAndConsEnv = (,) <$> use tyEnv <*> use consEnv

getValEnv :: (MonadState ReplState m) => m ValEnv
getValEnv = use valEnv

insertVal :: (MonadState ReplState m) => Var -> Val -> m ()
insertVal x v = valEnv %= insertValEnv x v

insertTy :: (MonadState ReplState m) => Var -> Scheme -> m ()
insertTy x sc = tyEnv %= insertTyEnv x sc

setEnvs :: (MonadState ReplState m) => ValEnv -> TyEnv -> ConstructorEnv -> m ()
setEnvs vEnv tEnv cEnv = do
  assign valEnv vEnv
  assign tyEnv tEnv
  assign consEnv cEnv
