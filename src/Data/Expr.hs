module Data.Expr where

import Common
import qualified Text.Show

type Var = String
type Tag = String

newtype Lit = LInt Int

data Expr =
    EVar Var
  | ETag Tag
  | ELit Lit
  | ELet [(Var,Expr)] Expr
  | EAbs Var Expr
  | EApp Expr Expr
  | ECase Expr [(Pat,Expr)]
  deriving Show

data Pat =
    PWildcard
  | PLit Lit
  | PCons Tag [Var]
  deriving Show

instance Show Lit where
  show (LInt n) = show n
