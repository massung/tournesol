{-# LANGUAGE OverloadedRecordDot #-}

module Tn.Expr
  ( Expr (..),
    exprParser,
  )
where

import qualified Data.Map.Strict as M
import Data.Symbol
import Text.Parsec
import Text.Parsec.Expr
import Text.Parsec.Token
import Tn.Error
import Tn.Function
import Tn.Parser
import Tn.Scalar
import Tn.Scope
import Tn.Units
import Prelude hiding (Infix, Prefix, (<|>))

data Expr
  = Ans
  | Term Scalar
  | Convert Units Expr
  | Unary (Scalar -> Either EvalError Scalar) Expr
  | Binary (Scalar -> Scalar -> Either EvalError Scalar) Expr Expr
  | Apply Function [Expr]

exprParser :: Parsec String Scope Expr
exprParser = buildExpressionParser exprTable exprTerm <|> (do eof; return Ans)

exprTerm :: Parsec String Scope Expr
exprTerm =
  exprParens
    <|> brackets lexer exprApply
    <|> (do reserved lexer "ans"; return Ans)
    <|> (do reserved lexer "true"; return $ Term 1)
    <|> (do reserved lexer "false"; return $ Term 0)
    <|> (scalarParser <&> Term)

exprParens :: Parsec String Scope Expr
exprParens = do
  expr <- parens lexer exprParser
  u <- optionMaybe unitsParser
  return $ case u of
    Nothing -> expr
    Just units -> Convert units expr

exprConvert :: ParsecT String Scope Identity Units
exprConvert = do
  reservedOp lexer ":" <|> reserved lexer "to"
  unitsParser

exprApply :: ParsecT String Scope Identity Expr
exprApply = do
  script <- getState
  funcName <- identifier lexer <&> intern
  xs <- sepBy exprParser (lexeme lexer $ char ';')
  case M.lookup funcName script._funcs of
    Just (f, _) -> return $ Apply f xs
    Nothing -> fail "unknown function"

exprTable :: OperatorTable String Scope Identity Expr
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

prefix :: String -> (Scalar -> Scalar) -> Operator String Scope Identity Expr
prefix op f = Prefix (do reservedOp lexer op; return $ Unary (Right . f))

binary :: String -> (Scalar -> Scalar -> Scalar) -> Assoc -> Operator String Scope Identity Expr
binary op f = Infix (do reservedOp lexer op; return $ Binary safeOp)
  where
    safeOp x y = Right $ f x y
