module SADT.Data.Val where

import qualified Data.Foldable as F
import qualified Data.Map as M
import qualified Data.String as String (unwords)
import SADT.Common
import SADT.Data.Expr
import qualified Text.Show

data Val
  = VTag Tag [Val]
  | VClo Var Expr ValEnv

instance Show Val where
  show (VTag t []) = t
  show (VTag t vs) = "(" ++ String.unwords (t : [show v | v <- vs]) ++ ")"
  show VClo {} = "<<closure>>"

newtype ValEnv = ValEnv (Map Var Val)
  deriving (Semigroup, Monoid)

lookupValEnv :: Var -> ValEnv -> Maybe Val
lookupValEnv x (ValEnv env) = M.lookup x env

insertValEnv :: Var -> Val -> ValEnv -> ValEnv
insertValEnv x v (ValEnv env) = ValEnv $ M.insert x v env

insertsValEnv :: [(Var, Val)] -> ValEnv -> ValEnv
insertsValEnv binds (ValEnv env) = ValEnv $ F.foldr' (\(x, v) e -> M.insert x v e) env (reverse binds)
