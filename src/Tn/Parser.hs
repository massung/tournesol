{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

module Tn.Parser
  ( module Tn.Lexer,
    dimParser,
    scalarParser,
    unitsParser,
  )
where

import qualified Data.Map.Strict as M
import Text.Parsec hiding ((<|>))
import Text.Parsec.Expr as Expr
import Text.Parsec.Token
import Tn.Dims
import Tn.Lexer
import Tn.Scalar
import Tn.Scope
import Tn.Symbol
import Tn.Unit
import Prelude hiding (Infix, Prefix, try)

dimParser :: Parsec String Scope Base
dimParser = do
  scope <- getState
  pos <- getPosition

  -- parse the dimension symbol
  s <- identifier lexer

  -- ensure the unit is in the state
  case M.lookup (intern s) scope._dims of
    Just dim -> return dim
    _ -> fail $ "unknown dimensions " ++ s ++ " at " ++ show pos

unitsOpTable :: (Ord a) => OperatorTable String Scope Identity (Dims a)
unitsOpTable =
  [ [ Infix (do unitsOp "*"; return (<>)) AssocLeft,
      Infix (do unitsOp "/"; return (</>)) AssocLeft
    ]
  ]
  where
    unitsOp op = try (do reservedOp lexer op; void $ lookAhead unitsTerm)

unitsParser :: Parsec String Scope Units
unitsParser = do
  units <- buildExpressionParser unitsOpTable (mconcat <$> many1 unitsTerm)
  if verifyUnits units
    then return units
    else fail "invalid units"

unitsTerm :: Parsec String Scope Units
unitsTerm = do
  scope <- getState

  -- parse the unit symbol and optional exponent
  s <- identifier lexer
  e <- exponentParser

  -- ensure the unit is in the state
  case M.lookup (intern s) scope._units of
    Just u -> return $ Dims [(u, e)]
    _ -> fail $ "unknown units: " ++ s

scalarParser :: Parsec String Scope Scalar
scalarParser = do
  x <- naturalOrFloat lexer

  -- optionally parse denominator if x is a natural number
  r <- case x of
    Left n -> option 1 (reservedOp lexer "%" >> integer lexer) <&> fromRational . (n %)
    Right n -> return n

  u <- optionMaybe $ try (unitsParser <|> unitsTerm)
  return $ Scalar r u
