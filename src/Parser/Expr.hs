{-# LANGUAGE OverloadedStrings #-}
module Parser.Expr where

import Common -- hiding (unwords)
import qualified Data.Foldable as F
import Data.Expr
-- import Data.String (unwords)
import Parser.SExpr

expectSym :: MonadFail m => SExpr -> m String
expectSym (SESym x) = return x
expectSym se = fail $ "symbol is expected, but not:" ++ show se

parseExpr :: MonadFail m => SExpr -> m Expr
parseExpr (SEInt n) = return $ ELit $ LInt n
parseExpr (SESym x) = return $ EVar x
parseExpr (SETag t) = return $ ETag t
parseExpr (SEBList _ses) = fail "not implemented for BList."
parseExpr (SEList content) = case content of
  [SESym "fn", SEBList ses, se] -> do
    e <- parseExpr se
    xs <- mapM expectSym ses
    return $ F.foldr' EAbs e xs
  [SESym "let", SEBList binds, se] -> ELet <$> parseBinds binds <*> parseExpr se
  se : ses -> do
    e <- parseExpr se
    es <- mapM parseExpr ses
    return $ F.foldl' EApp e es
  _ -> fail "parse failed."

parseBinds :: MonadFail m => [SExpr] -> m [(Var, Expr)]
parseBinds = mapM parseBind
  where
    parseBind :: MonadFail m => SExpr -> m (Var, Expr)
    parseBind (SEList [SESym x, se]) = (x,) <$> parseExpr se
    parseBind _ = fail "fail parseBind"

-- parseTuple :: MonadFail m => [SExpr] -> m Expr
-- parseTuple [] = fail "[] is invalid expr"
-- parseTuple [_] = fail "only one element tuple is not admitted."
-- parseTuple (x:y:ses) = do
--   ex <- parseExpr x
--   ey <- parseExpr y
--   es <- mapM parseExpr ses
--   return $ F.foldl' ETuple (ETuple ex ey) es

parsePat :: MonadFail m => SExpr -> m Pat
parsePat (SESym "_") = return PWildcard
parsePat (SEInt n) = return $ PLit (LInt n)
parsePat _ = fail "fail parsePat"

parseCase :: MonadFail m => SExpr -> m (Pat,Expr)
parseCase (SEList [p, e]) = (,) <$> parsePat p <*> parseExpr e
parseCase _ = fail "fail parseCase"

-- parseTuplePat :: MonadFail m => [SExpr] -> m Pat
-- parseTuplePat [] = fail "[] is invalid expr"
-- parseTuplePat [_] = fail "only one element tuple is not admitted."
-- parseTuplePat (x:y:ses) = do
--   px <- parsePat x
--   py <- parsePat y
--   ps <- mapM parsePat ses
--   return $ F.foldl' PTuple (PTuple px py) ps