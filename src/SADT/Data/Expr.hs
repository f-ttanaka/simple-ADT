module SADT.Data.Expr
  ( Expr (..),
    Case,
    Pat (..),
    Var,
    Tag,
  )
where

import SADT.Common

type Var = String

type Tag = String

data Expr
  = EVar Var
  | ETag Tag
  | EAbs Var Expr
  | EApp Expr Expr
  | ECase Expr [Case]
  deriving (Show)

type Case = (Pat, Expr)

data Pat
  = PWildcard
  | PVar Var
  | PCons Tag [Pat]
  deriving (Show)
