{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

module Tn.Parser
  ( module Tn.Lexer,
    dimsParser,
    scalarParser,
    unitsParser,
  )
where

import qualified Data.Map.Strict as M
import Data.Symbol
import Text.Parsec hiding ((<|>))
import Text.Parsec.Expr as Expr
import Text.Parsec.Token
import Tn.Dims
import Tn.Lexer
import Tn.Scalar
import Tn.Scope
import Tn.Units
import Prelude hiding (Infix, Prefix, try)

dimsParser :: Parsec String Scope Dims
dimsParser = buildExpressionParser unitsOpTable (mconcat <$> many1 dimsTerm)

dimsTerm :: Parsec String Scope Dims
dimsTerm = do
  scr <- getState
  pos <- getPosition

  -- parse the unit symbol and optional exponent
  d <- identifier lexer <&> intern
  e <- exponentParser

  -- ensure the unit is in the state
  case M.lookup d scr._dims of
    Nothing -> fail $ "unknown dimensions " ++ show d ++ " at " ++ show pos
    Just (d', _) -> return $ Dims [(d', e)]

unitsParser :: Parsec String Scope Units
unitsParser = do
  units <- buildExpressionParser unitsOpTable (mconcat <$> many1 unitsTerm)
  if verifyUnits units
    then return units
    else fail "invalid units"

unitsTerm :: Parsec String Scope Units
unitsTerm = do
  scr <- getState
  pos <- getPosition

  -- parse the unit symbol and optional exponent
  u <- identifier lexer <&> intern
  e <- exponentParser

  -- ensure the unit is in the state
  case M.lookup u scr._units of
    Nothing -> fail $ "unknown units " ++ show u ++ " at " ++ show pos
    Just (u', _) -> return $ Units [(u', e)]

scalarParser :: Parsec String Scope Scalar
scalarParser = do
  n <- naturalOrFloat lexer
  u <- optionMaybe $ try unitsParser <|> unitsTerm
  return $ case n of
    Left i -> Scalar (fromIntegral i) u
    Right f -> Scalar (toRational f) u
