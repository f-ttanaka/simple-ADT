{-# LANGUAGE GeneralisedNewtypeDeriving #-}
module Repl.Stmt where

import Common
import Control.Lens
import Core.Env
import Core.Expr
import Core.Parser
import Core.Type
import qualified Data.Set as S
import Control.Exception.Safe (MonadThrow, throwString)
import Repl.State

data Stmt =
    StExprDef Var Expr
  | StExpr Expr
  | StTyDef Tag (Set Uniq) [(Tag, [Type])]
  deriving Show

newtype Eval m a = Eval (StateT ReplState m a)
  deriving
    ( Functor
    , Applicative
    , Monad
    , MonadThrow
    , MonadIO
    , MonadState ReplState)

evalStmt :: (MonadIO m, MonadThrow m) => Stmt -> Eval m ()
evalStmt (StTyDef t us secs) = do
  mapM_ (evalConDef t us) secs
  te <- use consEnv
  print te
evalStmt _ = putStrLn "not implemented."

evalConDef :: (MonadIO m, MonadThrow m) 
  => Tag -> Set Uniq -> (Tag, [Type]) -> Eval m ()
evalConDef name us (t, tys) = do
  let ty = TyCon name [TyVar u | u <- S.toList us]
      sc = Forall us (foldr tyFunc ty tys)
  consEnv %= insertCEnv t sc

runEval :: MonadIO m => Eval m a -> m a
runEval (Eval m) = evalStateT m initialState