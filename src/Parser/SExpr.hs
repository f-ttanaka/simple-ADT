module Parser.SExpr where

import Common hiding ((<|>), many)
import           Text.Parsec.Text   (Parser)
import Data.Expr

import           Text.Parsec
import qualified Text.Parsec.Token       as Tok

data SExpr =
  SEInt Int
  | SESym Var
  | SETag Tag
  | SEBList [SExpr] -- bracket list
  | SEList [SExpr]
  deriving (Show, Eq)

identSymbol :: Parser Char
identSymbol = oneOf "!#$%&|*+-/<=>?@^_~"

lexerDef :: Tok.GenLanguageDef Text () Identity
lexerDef =  Tok.LanguageDef
  { Tok.commentStart    = "#|"
  , Tok.commentEnd      = "|#"
  , Tok.commentLine     = ";"
  , Tok.nestedComments  = True
  , Tok.identStart      = lower <|> identSymbol
  , Tok.identLetter     = alphaNum <|> identSymbol
  , Tok.opStart         = oneOf ""
  , Tok.opLetter        = oneOf ""
  , Tok.reservedNames   = []
  , Tok.reservedOpNames = []
  , Tok.caseSensitive   = True
  }

lexer :: Tok.GenTokenParser Text () Identity
lexer = Tok.makeTokenParser lexerDef

ident :: Parser Var
ident = Tok.identifier lexer

tag :: Parser Tag
tag = (:) <$> upper <*> many (Tok.identLetter lexerDef)

parens :: Parser a -> Parser a
parens = Tok.parens lexer

brackets :: Parser a -> Parser a
brackets = Tok.brackets lexer

natural :: Parser Int
natural = fromIntegral <$> Tok.natural lexer

textLiteral :: Parser String
textLiteral = Tok.stringLiteral lexer

sExpr :: Parser SExpr
sExpr = SEInt <$> natural
  -- <|> SEStr <$> textLiteral
  <|> SETag <$> tag
  <|> SESym <$> ident
  <|> SEBList <$> brackets (many sExpr)
  <|> SEList <$> parens (many sExpr)

parseSExpr :: MonadFail m => SourceName -> Text -> m SExpr
parseSExpr source txt = case parse sExpr source txt of
  Left err -> fail (show err)
  Right se -> return se

doParse :: MonadFail m => (SExpr -> m a) -> SourceName -> Text -> m a
doParse p src txt = do
  se <- parseSExpr src txt
  p se