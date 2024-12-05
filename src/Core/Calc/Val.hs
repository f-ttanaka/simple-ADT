module Core.Calc.Val where

import Common
import Core.Expr
import qualified Data.Foldable as F
import qualified Data.Map as M
import qualified Text.Show
import qualified Data.String as String (unwords)

data Val =
    VTag Tag [Val]
  | VClo Var Expr ValEnv

instance Show Val where
  show (VTag t vs) = t ++ String.unwords [show v | v <- vs]
  show VClo{} = "<<closure>>"

newtype ValEnv = ValEnv (Map Var Val)
  deriving (Semigroup, Monoid)

lookupValEnv :: Var -> ValEnv -> Maybe Val
lookupValEnv x (ValEnv env) = M.lookup x env

insertValEnv :: Var -> Val -> ValEnv -> ValEnv
insertValEnv x v (ValEnv env) = ValEnv $ M.insert x v env

insertsValEnv :: [(Var, Val)] -> ValEnv -> ValEnv
insertsValEnv binds (ValEnv env) = ValEnv $ F.foldr' (\(x,v) e -> M.insert x v e) env (reverse binds)