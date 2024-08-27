{-# LANGUAGE NegativeLiterals #-}
{-# LANGUAGE OverloadedStrings #-}

module Tn.Lexer where

import Text.Parsec
import Text.Parsec.Expr
import Text.Parsec.Token
import Tn.Dims
import Tn.Script
import Prelude hiding (Infix, (<|>))

lexer :: GenTokenParser String Script Identity
lexer = makeTokenParser lang
  where
    lang =
      LanguageDef
        { commentStart = "",
          commentEnd = "",
          commentLine = "#",
          nestedComments = False,
          identStart = letter,
          identLetter = letter,
          opStart = oneOf ":$%?+-*/^<>=!@|&",
          opLetter = oneOf "+-*/<>=!@|&",
          reservedNames = ["ans", "base", "dim", "func", "inf", "si", "storage", "to", "unit"],
          reservedOpNames = [":", "+", "-", "*", "/", "^", "=", "<", ">", "<=", ">=", "<>"],
          caseSensitive = True
        }

rationalParser :: Parsec String Script Rational
rationalParser = either fromInteger toRational <$> naturalOrFloat lexer

exponentParser :: Parsec String Script Rational
exponentParser = option 1 $ do
  _ <- lexeme lexer $ char '^'
  s <- signParser
  e <- rationalParser
  return $ e * s

signParser :: Parsec String Script Rational
signParser = option 1 (neg <|> pos)
  where
    neg = reservedOp lexer "-" >> return (-1)
    pos = reservedOp lexer "+" >> return 1

unitsOpTable :: (Disjoin a) => OperatorTable String Script Identity a
unitsOpTable =
  [ [ Infix (do reservedOp lexer "*"; return (<>)) AssocLeft,
      Infix (do reservedOp lexer "/"; return (</>)) AssocLeft
    ]
  ]
