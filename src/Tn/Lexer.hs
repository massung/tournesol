{-# LANGUAGE NegativeLiterals #-}
{-# LANGUAGE OverloadedStrings #-}

module Tn.Lexer where

import Text.Parsec
import Text.Parsec.Token
import Tn.Script

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
          reservedNames = ["as", "base", "dim", "func", "inf", "si", "storage", "to", "unit"],
          reservedOpNames = [":", "+", "-", "*", "/", "^", "=", "<", ">", "<=", ">=", "<>"],
          caseSensitive = True
        }
