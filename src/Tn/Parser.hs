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
import Tn.Error
import Tn.Expr
import Tn.Lexer
import Tn.Scalar
import Tn.Script
import Tn.Units
import Prelude hiding (Infix, Prefix, try)

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

unitsExprTable :: OperatorTable String Script Identity Units
unitsExprTable =
  [ [ Expr.Infix (do reservedOp lexer "*"; return (<>)) AssocLeft,
      Expr.Infix (do reservedOp lexer "/"; return (</>)) AssocLeft
    ]
  ]

unitsParser :: Parsec String Script Units
unitsParser = do
  units <- buildExpressionParser unitsExprTable (mconcat <$> many1 unitsTerm)
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

putScript :: Either String Script -> Parsec String Script ()
putScript (Left err) = fail err
putScript (Right scr) = putState scr

baseUnits :: Dim -> Parsec String Script ()
baseUnits dim = do
  reserved lexer "base"

  -- base unit name
  pos <- getPosition
  si <- optionMaybe $ reserved lexer "si"
  name <- identifier lexer

  -- create the base unit and optional derived units
  let base = Unit {_symbol = intern name, _name = name, _dim = dim, _conv = Base}
      units = if isJust si then siUnits base else [base]

  -- declare all the units
  getState <&> declUnits units (Just pos) >>= putScript

exprParser :: Parsec String Script Expr
exprParser = buildExpressionParser exprTable exprTerm

exprTerm :: Parsec String Script Expr
exprTerm = do
  exprParens
    -- <|> brackets lexer exprApply
    <|> Term
    <$> scalarParser
    <|> (do reserved lexer "true"; return $ Term 1)
    <|> (do reserved lexer "false"; return $ Term 0)

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

-- exprApply = do
--   script <- getState
--   s <- identifier lexer
--   xs <- sepBy exprParser (lexeme lexer $ char ';')
--   case M.lookup s $ funcs script of
--     Just f -> return $ Call f xs
--     Nothing -> fail $ s ++ " ?"

exprTable :: OperatorTable String Script Identity Expr
exprTable =
  [ [prefix "-" negate, prefix "+" id],
    -- [binary "^" powScalar AssocLeft],
    [binary "*" (*) AssocLeft, binary "/" (/) AssocLeft],
    [binary "+" (+) AssocLeft, binary "-" (-) AssocLeft],
    [binary "==" (cmpOp (==)) AssocLeft, binary "/=" (cmpOp (/=)) AssocLeft, binary "<" (cmpOp (<)) AssocLeft, binary ">" (cmpOp (>)) AssocLeft, binary "<=" (cmpOp (<=)) AssocLeft, binary ">=" (cmpOp (>=)) AssocLeft],
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
