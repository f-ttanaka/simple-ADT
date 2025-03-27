module SADT.Data.Stmt (Stmt (..)) where

import SADT.Common
import SADT.Data.Expr
import SADT.Data.Type

data Stmt
  = StExprDef Var Expr
  | StExpr Expr
  | StTyDef Tag (Set Uniq) [(Tag, [Type])]
  deriving (Show)
