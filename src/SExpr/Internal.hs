module SExpr.Internal
  ( SExpr(..) )
where

import Common (Eq, Show, String)

data SExpr =
  SESym String
  | SETag String
  | SEBList [SExpr] -- bracket list
  | SEList [SExpr]
  deriving (Show, Eq)