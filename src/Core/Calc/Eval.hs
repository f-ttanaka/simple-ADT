module Core.Calc.Eval where

import Common
import Core.Env
import Core.Expr
import Core.Calc.Val

type EvalEnv = (ConstructorEnv, ValEnv)

newtype Eval m a = Eval {unWrapEval :: ReaderT ValEnv m a}
  deriving
    ( Functor
    , Applicative
    , Monad
    , MonadReader ValEnv
    , MonadThrow)

eval :: MonadThrow m => Expr -> Eval m Val
eval (EVar x) = do
  env <- ask
  case lookupValEnv x env of
    Just v -> return v
    _ -> throwString $ "variable " ++ show x ++ " is not in env."
eval (ETag t) = return $ VTag t []
eval (EAbs x bod) = VClo x bod <$> ask
eval (EApp eFun eArg) = do
  vFun <- eval eFun
  case vFun of
    VTag t vs -> do
      vArg <- eval eArg
      return $ VTag t (vs ++ [vArg])
    VClo x eBody clo -> do
      vArg <- eval eArg
      local (const $ insertValEnv x vArg clo) (eval eBody)
eval (ECase e cs) = do
  v <- eval e
  evalMatch v cs

evalMatch :: MonadThrow m => Val -> [Case] -> Eval m Val
evalMatch _ [] = throwString "failed to pattern-match."
evalMatch val ((pat,eCase):cs) = case (val, pat) of
  (VTag tagV vs, PCons tagP xs)
    | tagV == tagP -> local (insertsValEnv $ zip xs vs) (eval eCase)
  (_, PWildcard) -> eval eCase
  _ -> evalMatch val cs

runEval :: MonadCatch m => Expr -> ValEnv -> m Val
runEval e = runReaderT $ unWrapEval (eval e)