module Semantics.Eval where

import Control.Monad.Check
import           Control.Monad.Reader
import           Data.Expr
import qualified Data.Map             as M

data Val = VInt Int
  | VBool Bool
  -- 変数がある場合は関数のクロージャ
  | VClosure (Maybe Name) Expr VEnv
  | VPrim (Expr -> Eval Val)

instance Show Val where
  show (VInt n)      = show n
  show (VBool True)  = "true"
  show (VBool False) = "false"
  show VClosure{}    = "<closure>"
  show VPrim{}       = "<primitive>"

type VEnv = M.Map Name Val
type Eval a = ReaderT VEnv Check a

unpackInt :: Expr -> Eval Int
unpackInt ex = do
  v <- eval ex
  case v of
    VInt n -> return n
    _      -> fail $ "unpackInt " ++ show ex

unpackBool :: Expr -> Eval Bool
unpackBool ex = do
  v <- eval ex
  case v of
    VBool b -> return b
    _       -> fail $ "unpackBool " ++ show ex

unpackFunc :: Expr -> Eval (Name, Expr, VEnv)
unpackFunc ex = do
  v <- eval ex
  case v of
    VClosure (Just x) bod clo -> return (x,bod,clo)
    _                         -> fail $ "unpackFunc " ++ show ex

toClosure :: VEnv -> Expr -> Val
toClosure venv (EAbs x ex) = VClosure (Just x) ex venv
toClosure venv e           = VClosure Nothing e venv

evalLit :: Lit -> Eval Val
evalLit (LInt n) = return $ VInt n

evalApp :: Val -> Expr -> Eval Val
evalApp (VClosure (Just x) bod clo) ex = do
  arg <- eval ex
  local (const $ M.insert x arg clo) (eval bod)
evalApp (VPrim prim) ex = prim ex
evalApp ef _ = fail $ "evalApp " ++ show ef

eval :: Expr -> Eval Val
eval (ELit l)  = evalLit l
eval (EVar x) = do
  env <- ask
  case M.lookup x env of
    Nothing                       -> fail $ x ++ " is not in val env"
    Just (VClosure Nothing e clo) -> local (const clo) (eval e)
    Just v                        -> return v
eval (EAbs x bod) = asks $ VClosure (Just x) bod
eval (EApp e1 e2) = do
  v1 <- eval e1
  evalApp v1 e2
eval (ELet defs bod) = do
  env <- ask
  let env' = M.union (M.fromList [(x, toClosure env' e) | (x,e) <- defs]) env
  local (const env') (eval bod)

runEval :: Eval Val -> VEnv -> Check Val
runEval = runReaderT
