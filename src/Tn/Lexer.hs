{-# LANGUAGE NegativeLiterals #-}
{-# LANGUAGE OverloadedStrings #-}

module Tn.Lexer where

import Text.Parsec
import Text.Parsec.Expr
import Text.Parsec.Token
import Tn.Dims
import Tn.Scope
import Prelude hiding (Infix, (<|>))

lexer :: GenTokenParser String Scope Identity
lexer = makeTokenParser lang
  where
    lang =
      LanguageDef
        { commentStart = "",
          commentEnd = "",
          commentLine = "#",
          nestedComments = False,
          identStart = letter <|> char '_',
          identLetter = letter,
          opStart = oneOf ":$%?+-*/^<>=!@|&",
          opLetter = oneOf "+-*/<>=!@|&",
          reservedNames = ["_", "ans", "base", "const", "dim", "function", "inf", "si", "storage", "unit"],
          reservedOpNames = [":", "+", "-", "*", "/", "^", "=", "<", ">", "<=", ">=", "<>"],
          caseSensitive = True
        }

rationalParser :: Parsec String Scope Rational
rationalParser = either fromInteger toRational <$> naturalOrFloat lexer

exponentParser :: Parsec String Scope Rational
exponentParser = option 1 $ do
  _ <- lexeme lexer $ char '^'
  s <- signParser
  e <- rationalParser
  return $ e * s

signParser :: Parsec String Scope Rational
signParser = option 1 (neg <|> pos)
  where
    neg = reservedOp lexer "-" >> return (-1)
    pos = reservedOp lexer "+" >> return 1

unitsOpTable :: (Disjoin a) => OperatorTable String Scope Identity a
unitsOpTable =
  [ [ Infix (do reservedOp lexer "*"; return (<>)) AssocLeft,
      Infix (do reservedOp lexer "/"; return (</>)) AssocLeft
    ]
  ]
