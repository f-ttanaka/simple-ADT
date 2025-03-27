{-# LANGUAGE GeneralisedNewtypeDeriving #-}

module SADT.Repl.Stmt where

import Control.Lens
import qualified Data.Set as S
import SADT.Common
import SADT.Data.Env
import SADT.Data.Expr
import SADT.Data.Stmt
import SADT.Data.Type
import SADT.Data.Val
import SADT.Eval
import SADT.Repl.State
import SADT.Typing.Infer

newtype Exec m a = Exec (StateT ReplState m a)
  deriving
    ( Functor,
      Applicative,
      Monad,
      MonadThrow,
      MonadCatch,
      MonadMask,
      MonadIO,
      MonadState ReplState
    )

evalStmt :: (MonadIO m, MonadCatch m) => Stmt -> Exec m ()
evalStmt (StTyDef t us secs) = mapM_ (evalConDef t us) secs
evalStmt (StExprDef x e) = do
  (vEnv, tEnv, cEnv) <- getEnvs
  sc <- runInfer e tEnv cEnv [x]
  let tEnv' = insertTyEnv x sc tEnv
  case e of
    EAbs xAbs eBody -> do
      let vEnv' = insertValEnv x (VClo xAbs eBody vEnv') vEnv
      setEnvs vEnv' tEnv' cEnv
    _ -> do
      v <- runEval e vEnv
      let vEnv' = insertValEnv x v vEnv
      setEnvs vEnv' tEnv' cEnv
evalStmt (StExpr e) = do
  vEnv <- getValEnv
  v <- runEval e vEnv
  print v

evalConDef :: (Monad m) => Tag -> Set Uniq -> (Tag, [Type]) -> Exec m ()
evalConDef name us (t, tys) = do
  let ty = TyCon name [TyVar u | u <- S.toList us]
      sc = Forall us (foldr tyFunc ty tys)
  consEnv %= insertCEnv t sc name

runExec :: (MonadIO m) => Exec m a -> m a
runExec (Exec m) = evalStateT m initialState
