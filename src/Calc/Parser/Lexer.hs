{-# LANGUAGE NegativeLiterals #-}
{-# LANGUAGE OverloadedStrings #-}

module Calc.Parser.Lexer where

import Data.Functor.Identity
import Text.Parsec
import Text.Parsec.Token

lexer :: GenTokenParser String st Identity
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
          opStart = oneOf ":+-*/<>=",
          opLetter = oneOf "=",
          reservedNames = ["_", "any", "none", "to", "si", "units", "function", "true", "false"],
          reservedOpNames = ["+", "-", "*", "/", "<", ">", "<=", ">=", "=", ":"],
          caseSensitive = True
        }

-- shared parsers ---------------------------------

parseExponent = do
  reservedOp lexer "^"
  s <- parseSign
  e <- either fromInteger toRational <$> naturalOrFloat lexer
  return (e * s)

parseSign = option 1 (neg <|> pos)
  where
    neg = reservedOp lexer "-" >> return -1
    pos = reservedOp lexer "+" >> return 1
