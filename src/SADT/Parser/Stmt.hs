module SADT.Parser.Stmt
  ( parseStmt,
    getFileContents,
  )
where

import qualified Data.Map as M
import qualified Data.Set as S
import Data.Text.Lazy (Text, pack)
import SADT.Common hiding (Text, many, try, (<|>))
import SADT.Data.Expr
import SADT.Data.Stmt
import SADT.Data.Type
import SADT.Parser.Expr
import SADT.Parser.Internal
import System.IO
import Text.Parsec
import Text.Parsec.Text.Lazy (Parser)

exprDef :: Parser Stmt
exprDef = do
  reserved "def"
  f <- identifier
  xs <- many identifier
  reservedOp "="
  e <- expr
  return $ StExprDef f (foldr EAbs e xs)

type TyVarMap = Map Var Uniq

typeDef :: Parser Stmt
typeDef = do
  reserved "data"
  t <- tag
  args <- many identifier
  reservedOp "="
  if hasDuplicates args
    then fail $ "there exists duplicated variables in " ++ show args
    else
      let seqn = [0 .. (length args - 1)]
          tvMap = M.fromList $ zip args seqn
       in StTyDef t (S.fromList seqn) <$> sepBy1 (prodType tvMap) (reservedOp "|")
  where
    prodType :: TyVarMap -> Parser (Tag, [Type])
    prodType tvm = (,) <$> tag <*> many (typ tvm)

typ :: TyVarMap -> Parser Type
typ tvm =
  (TyVar <$> tyVar)
    <|> (TyCon <$> tag <*> many (typ tvm))
  where
    tyVar :: Parser Uniq
    tyVar = do
      x <- identifier
      case M.lookup x tvm of
        Just i -> return i
        _ -> fail $ show x ++ " is rigit type variable."

stmt :: Parser Stmt
stmt = p <* reservedOp ";"
  where
    p = try (StExpr <$> expr) <|> exprDef <|> typeDef

parseStmt :: (MonadThrow m) => SourceName -> String -> m Stmt
parseStmt src t = case parse stmt src (pack t) of
  Right st -> return st
  Left err -> throwString $ show err

getFileContents :: FilePath -> IO [Stmt]
getFileContents fp = withFile fp ReadMode $ \hdl -> do
  c <- hGetContents hdl
  case parse (many stmt) fp (pack c) of
    Right sts -> return sts
    Left err -> throwString $ show err
