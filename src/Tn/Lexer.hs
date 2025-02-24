{-# LANGUAGE NegativeLiterals #-}
{-# LANGUAGE OverloadedStrings #-}

module Tn.Lexer where

import Text.Parsec
import Text.Parsec.Token
import Tn.Scope
import Prelude hiding (Infix, try, (<|>))

lexer :: GenTokenParser String Scope Identity
lexer = makeTokenParser lang
  where
    lang =
      LanguageDef
        { commentStart = "\"",
          commentEnd = "\"",
          commentLine = "#",
          nestedComments = False,
          identStart = letter <|> char '_',
          identLetter = letter,
          opStart = oneOf "^+-*/<>=~%",
          opLetter = oneOf "^+-*/<>=",
          reservedNames = ["_", "ans", "binary", "const", "dim", "english", "function", "imperial", "si", "unit"],
          reservedOpNames = ["+", "-", "*", "/", "^", "=", "==", "~=", "<", ">", "<=", ">=", "<>"],
          caseSensitive = True
        }

rationalParser :: Parsec String Scope Double
rationalParser = either fromInteger id <$> naturalOrFloat lexer

exponentParser :: Parsec String Scope Int
exponentParser = option 1 $ do
  _ <- lexeme lexer $ char '^'
  s <- signParser
  n <- natural lexer <&> fromInteger
  return $ n * s

signParser :: Parsec String Scope Int
signParser = option 1 (neg <|> pos)
  where
    neg = reservedOp lexer "-" >> return (-1)
    pos = reservedOp lexer "+" >> return 1
