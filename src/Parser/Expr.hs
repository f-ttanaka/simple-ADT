module Parser.Expr where

import qualified Data.Foldable as F
import Data.Expr
import Parser.SExpr
import Data.Text (Text)
import Text.Parsec (SourceName)

expectSym :: MonadFail m => SExpr -> m String
expectSym (SESym x) = return x
expectSym se = fail $ unwords ["symbol is expected, but not:", show se]

parseExpr :: MonadFail m => SExpr -> m Expr
parseExpr (SEInt n) = return $ ELit $ LInt n
parseExpr (SESym x) = return $ EVar x
-- parseExpr (SETag t) = return $ EVar t
parseExpr (SEBList _) = fail "List is yet defined."
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

parseBinds :: MonadFail m => [SExpr] -> m [(Name, Expr)]
parseBinds = mapM parseBind
  where
    parseBind :: MonadFail m => SExpr -> m (Name, Expr)
    parseBind (SEList [SESym x, se]) = (x,) <$> parseExpr se
    parseBind _ = fail "fail parseBind"