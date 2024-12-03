module Core.Calc.Eval where

import Common
import Core.Env
import Core.Expr
import Core.Calc.Val

type EvalEnv = (ConstructorEnv, ValEnv)

newtype Eval m a = Eval (ReaderT EvalEnv m a)
  deriving
    ( Functor
    , Applicative
    , Monad
    , MonadReader EvalEnv
    , MonadThrow)

eval :: MonadThrow m => Expr -> Eval m Val
eval (EVar x) = do
  env <- snd <$> ask
  case lookupValEnv x env of
    Just v -> return v
    _ -> throwString $ "variable " ++ show x ++ " is not in env."
eval (ETag t) = do
  cEnv <- fst <$> ask
  undefined