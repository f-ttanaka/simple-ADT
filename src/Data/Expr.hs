module Data.Expr where

import Data.Type

type Name = String

data Lit = LInt Int

data Expr =
    EVar Name
  | ELit Lit
  | ELet [(Name,Expr)] Expr
  | EAbs Name Expr
  | EApp Expr Expr
  deriving Show

data Pat = PVar Name
  | PWildcard
  | PLit Lit
  | PCon Scheme [Pat]

instance Show Lit where
  show (LInt n) = show n
