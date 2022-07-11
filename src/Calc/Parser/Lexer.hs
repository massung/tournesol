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
          commentLine = "",
          nestedComments = False,
          identStart = letter,
          identLetter = letter,
          opStart = oneOf "_:+-*/",
          opLetter = parserZero,
          reservedNames = ["ans", "to"],
          reservedOpNames = ["_", "+", "-", "*", "/", ":"],
          caseSensitive = True
        }