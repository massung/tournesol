{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

module Tn.Parser
  ( module Tn.Lexer,
    exprParser,
    scalarParser,
    unitsParser,
    prefix,
    binary,
  )
where

import qualified Data.Map.Strict as M
import Data.Symbol
import Text.Parsec hiding ((<|>))
import Text.Parsec.Expr as Expr
import Text.Parsec.Token
import Tn.Conv
import Tn.Dims
import Tn.Expr
import Tn.Lexer
import Tn.Scalar
import Tn.Script
import Tn.Units
import Prelude hiding (Infix, Prefix, try)

dimsParser :: Parsec String Script Dims
dimsParser = buildExpressionParser unitsOpTable (mconcat <$> many1 dimsTerm)

dimsTerm :: Parsec String Script Dims
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

unitsParser :: Parsec String Script Units
unitsParser = do
  units <- buildExpressionParser unitsOpTable (mconcat <$> many1 unitsTerm)
  if verifyUnits units
    then return units
    else fail "invalid units"

unitsTerm :: Parsec String Script Units
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

scalarParser :: Parsec String Script Scalar
scalarParser = do
  n <- naturalOrFloat lexer
  u <- optionMaybe $ try unitsParser <|> unitsTerm
  return $ case n of
    Left i -> Scalar (fromIntegral i) u
    Right f -> Scalar (toRational f) u

exprParser :: Parsec String Script Expr
exprParser = buildExpressionParser exprTable exprTerm <|> (do eof; return Ans)

exprTerm :: Parsec String Script Expr
exprTerm =
  exprParens
    <|> brackets lexer exprApply
    <|> (do reserved lexer "ans"; return Ans)
    <|> (do reserved lexer "true"; return $ Term 1)
    <|> (do reserved lexer "false"; return $ Term 0)
    <|> (scalarParser <&> Term)

exprParens :: Parsec String Script Expr
exprParens = do
  expr <- parens lexer exprParser
  u <- optionMaybe unitsTerm
  return $ case u of
    Nothing -> expr
    Just units -> Convert units expr

exprConvert :: ParsecT String Script Identity Units
exprConvert = do
  reservedOp lexer ":" <|> reserved lexer "to"
  unitsParser

exprApply :: ParsecT String Script Identity Expr
exprApply = do
  script <- getState
  funcName <- identifier lexer <&> intern
  xs <- sepBy exprParser (lexeme lexer $ char ';')
  case M.lookup funcName script._funcs of
    Just (f, _) -> return $ Apply f xs
    Nothing -> fail "unknown function"

exprTable :: OperatorTable String Script Identity Expr
exprTable =
  [ [prefix "-" negate, prefix "+" id],
    [binary "^" powScalar AssocLeft],
    [binary "*" (*) AssocLeft, binary "/" (/) AssocLeft],
    [binary "+" (+) AssocLeft, binary "-" (-) AssocLeft],
    [binary "=" (cmpOp (==)) AssocLeft, binary "/=" (cmpOp (/=)) AssocLeft, binary "<" (cmpOp (<)) AssocLeft, binary ">" (cmpOp (>)) AssocLeft, binary "<=" (cmpOp (<=)) AssocLeft, binary ">=" (cmpOp (>=)) AssocLeft],
    [Postfix (do Convert <$> exprConvert)]
  ]
  where
    cmpOp f x y = if f x y then 1 else 0

prefix :: String -> (Scalar -> Scalar) -> Operator String Script Identity Expr
prefix op f = Prefix (do reservedOp lexer op; return $ Unary (Right . f))

binary :: String -> (Scalar -> Scalar -> Scalar) -> Assoc -> Operator String Script Identity Expr
binary op f = Infix (do reservedOp lexer op; return $ Binary safeOp)
  where
    safeOp x y = Right $ f x y
