module SADT.Parser.Internal where

import Data.Text.Internal.Lazy (Text)
import SADT.Common hiding (Op, Text, many, (<|>))
import Text.Parsec
import qualified Text.Parsec.Expr as Ex
import Text.Parsec.Text.Lazy (Parser)
import qualified Text.Parsec.Token as Tok

reservedNames :: [String]
reservedNames =
  [ "def",
    "data",
    "fn",
    "match",
    "with"
  ]

reservedOps :: [String]
reservedOps =
  [ "=>",
    "->",
    "-",
    "*",
    "+",
    "=",
    "==",
    "<",
    "|",
    ";"
  ]

identLetter :: Parser Char
identLetter = alphaNum <|> oneOf "_'"

lexer :: Tok.GenTokenParser Text () Identity
lexer =
  Tok.makeTokenParser $
    Tok.LanguageDef
      { Tok.commentStart = "{-",
        Tok.commentEnd = "-}",
        Tok.commentLine = "--",
        Tok.nestedComments = True,
        Tok.identStart = lower <|> char '_',
        Tok.identLetter = identLetter,
        Tok.opStart = oneOf ":!#$%&*+./<=>?@\\^|-~",
        Tok.opLetter = oneOf ":!#$%&*+./<=>?@\\^|-~",
        Tok.reservedNames = reservedNames,
        Tok.reservedOpNames = reservedOps,
        Tok.caseSensitive = True
      }

reserved :: String -> Parser ()
reserved = Tok.reserved lexer

reservedOp :: String -> Parser ()
reservedOp = Tok.reservedOp lexer

identifier :: Parser String
identifier = Tok.identifier lexer

tag :: Parser String
tag = Tok.lexeme lexer $ (:) <$> upper <*> many identLetter

symbol :: String -> Parser ()
symbol str = void $ Tok.symbol lexer str

parens :: Parser a -> Parser a
parens = Tok.parens lexer

brackets :: Parser a -> Parser a
brackets = Tok.brackets lexer

commaSep :: Parser a -> Parser [a]
commaSep = Tok.commaSep lexer

semi :: Parser String
semi = Tok.semi lexer

spaces :: Parser ()
spaces = skipMany1 space

natural :: Parser Integer
natural = Tok.natural lexer

type Op a = Ex.Operator Text () Identity a

type Operators a = Ex.OperatorTable Text () Identity a

infixOp :: String -> (a -> a -> a) -> Ex.Assoc -> Op a
infixOp x op = Ex.Infix (reservedOp x >> return op)

infixOpAL :: String -> (a -> a -> a) -> Op a
infixOpAL x op = infixOp x op Ex.AssocLeft

contents :: Operators a -> Parser a -> Parser a
contents = Ex.buildExpressionParser
