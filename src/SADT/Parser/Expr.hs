module SADT.Parser.Expr
  ( parseExpr,
    expr,
  )
where

import Data.Foldable (foldl)
import Data.Text.Lazy (pack)
import SADT.Common hiding (many, (<|>))
import SADT.Data.Expr
import SADT.Parser.Internal
import Text.Parsec
import Text.Parsec.Text.Lazy (Parser)

-- letBind :: Parser (Var, Expr)
-- letBind = do
--   f <- identifier
--   xs <- many identifier
--   reservedOp "="
--   e <- expr
--   return (f, foldr' EAbs e xs)

-- letExpr :: Parser Expr
-- letExpr =
--   LetExpr
--     <$> (reserved "let" *> option False (reserved "rec" >> return True))
--     <*> sepBy1 letBind (symbol "and")
--     <*> (reserved "in" *> expr)

lambda :: Parser Expr
lambda =
  EAbs
    <$> (reserved "fn" *> identifier <* reservedOp "=>")
    <*> expr

varPat :: Parser Pat
varPat = do
  x <- identifier
  return $ case x of
    "_" -> PWildcard
    _ -> PVar x

consPat :: Parser Pat
consPat = PCons <$> tag <*> many pat

pat :: Parser Pat
pat = varPat <|> consPat

caseP :: Parser (Pat, Expr)
caseP = do
  reserved "|"
  p <- pat
  reserved "->"
  e <- expr
  return (p, e)

matchP :: Parser Expr
matchP = do
  e <- reserved "match" >> expr <* reserved "with"
  cs <- many1 caseP
  return $ ECase e cs

aexpr :: Parser Expr
aexpr =
  parens expr
    <|> matchP
    <|> lambda
    <|> (ETag <$> tag)
    <|> (EVar <$> identifier)

term :: Parser Expr
term =
  aexpr >>= \x ->
    (many1 aexpr >>= \xs -> return (foldl EApp x xs))
      <|> return x

table :: Operators Expr
table = []

-- [ [ infixOpAL "*" (\e1 e2 -> Prim (Arith (*) e1 e2))
--   ],
--   [ infixOpAL "+" (\e1 e2 -> Prim (Arith (+) e1 e2)),
--     infixOpAL "-" (\e1 e2 -> Prim (Arith (-) e1 e2))
--   ],
--   [ infixOpAL "==" (\e1 e2 -> Prim (Comp (==) e1 e2)),
--     infixOpAL "<" (\e1 e2 -> Prim (Comp (<) e1 e2))
--   ]
-- ]

expr :: Parser Expr
expr = contents table term

parseExpr :: (MonadCatch m) => SourceName -> String -> m Expr
parseExpr src t = case parse expr src (pack t) of
  Right e -> return e
  Left err -> throwString $ show err
