module Repl.Parser where

import Common hiding (seq)

import Control.Exception.Safe (MonadThrow, throwString)
import Core.Expr
import Core.Parser
import Core.Type
import qualified Data.Map as M
import qualified Data.Set as S
import SExpr.Internal
import SExpr.Parser
import Repl.Stmt

type TyVarMap = Map Var Uniq

parseStmt :: MonadThrow m => SExpr -> m Stmt
parseStmt (SEList [SESym "def", SESym x, se]) = StExprDef x <$> parseExpr se
parseStmt (SEList [SESym "defn", SESym f, SEBList xs, se]) = do
  e <- parseExpr se
  args <- mapM expectSym xs
  return $ StExprDef f (foldr EAbs e args)
parseStmt (SEList (SESym "data" : SETag t : SEBList seArgs : seSums)) = do
  args <- mapM expectSym seArgs
  if hasDuplicates args 
    then throwString $ "there exists duplicated variables in " ++ show args 
    else (let seq = [0..(length args - 1)]
              tvMap = M.fromList $ zip args seq
          in StTyDef t (S.fromList seq) <$> mapM (parseTagAndArgs tvMap) seSums)
parseStmt se = StExpr <$> parseExpr se

parseTagAndArgs :: MonadThrow m => TyVarMap -> SExpr -> m (Tag, [Type])
parseTagAndArgs tvMap (SEList [SETag t, SEBList ses]) =
  (t,) <$> mapM (parseType tvMap) ses
parseTagAndArgs _ se =
  throwString $ "failed to parse constructor definition: " ++ show se

parseType :: MonadThrow m => TyVarMap -> SExpr -> m Type
parseType tvMap (SESym x)
  | Just i <- M.lookup x tvMap = return $ TyVar i
  | otherwise = throwString $ show x ++ " is rigit type variable."
parseType _ (SETag t) = return $ TyCon t []
parseType tvMap (SEList (SETag t : ses)) = TyCon t <$> mapM (parseType tvMap) ses
parseType _ se = throwString $ "failed to parse to type: " ++ show se