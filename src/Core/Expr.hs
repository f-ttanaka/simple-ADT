{-# LANGUAGE GADTs #-}
module Core.Expr
  ( Expr(..)
  , Pat(..)
  , Var
  , Tag
  ) where

import Common

type Var = String
type Tag = String

data Expr =
    EVar Var
  | ETag Tag
  | EAbs Var Expr
  | EApp Expr Expr
  | ECase Expr [(Pat, Expr)]
  deriving Show

data Pat =
    PWildcard
  | PCons Tag [Var]
  deriving Show
