module Repl.Stmt where

import Control.Monad.Reader(MonadReader(..))
import Data.Expr
import qualified Data.Foldable as F
import qualified Data.Map as M
import Data.Type
import Parser.Expr
import Parser.SExpr
import Semantics.Eval
import Typing.Infer

data Stmt =
  DefStmt Name Expr
  | ExprStmt Expr
  deriving Show

parseStmt :: MonadFail m => SExpr -> m Stmt
parseStmt (SEList [SESym "def", SESym x, se]) = DefStmt x <$> parseExpr se
parseStmt (SEList [SESym "defn", SESym f, SEBList xs, se]) = do
  e <- parseExpr se
  args <- mapM expectSym xs
  return $ DefStmt f (F.foldr' EAbs e args)
parseStmt se = ExprStmt <$> parseExpr se

inferDef :: Name -> Expr -> Infer Scheme
inferDef x e = do
  a <- freshVar
  local (M.insert x (Forall mempty a)) (inferScheme e)

evalStmt :: Stmt -> Eval Val
evalStmt (DefStmt x e) = do
  venv <- ask
  let venv' = M.insert x (toClosure venv' e) venv
  local (M.insert x (toClosure venv e)) (eval e)
evalStmt (ExprStmt e) = eval e
