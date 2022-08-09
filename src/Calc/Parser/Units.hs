module Calc.Parser.Units where

import Calc.Parser.Lexer
import Calc.Script
import Calc.Units
import Data.Map.Strict as M
import Text.Parsec
import Text.Parsec.Expr
import Text.Parsec.Token

unitsParser :: Parsec String Script Units
unitsParser = buildExpressionParser unitsExprTable unitsTerm

unitsTerm = parens lexer unitsParser <|> terms
  where
    terms = do
      units <- mconcat <$> many1 unitTerm
      if validateUnits units
        then return units
        else fail "illegal units"

unitTerm = do
  ident <- identifier lexer
  script <- getState
  case units script !? ident of
    Nothing -> fail $ "unknown unit: " ++ ident
    Just unit -> Units . M.singleton unit <$> exponentParser

unitsExprTable =
  [ [ Infix (do reservedOp lexer "*"; return (<>)) AssocLeft,
      Infix (do reservedOp lexer "/"; return (</>)) AssocLeft
    ]
  ]
