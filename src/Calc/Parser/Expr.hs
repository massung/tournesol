module Calc.Parser.Expr where

import Calc.Expr
import Calc.Parser.Lexer
import Calc.Parser.Scalar
import Calc.Parser.Units
import Calc.Scalar
import Calc.Script
import Data.Map.Strict as M
import Text.Parsec
import Text.Parsec.Expr
import Text.Parsec.Token

exprParser :: Parsec String Script Expr
exprParser = buildExpressionParser exprTable exprTerm

exprTerm = do
  exprParens
    <|> brackets lexer exprApply
    <|> Term <$> (scalarParser <|> scalarSingleton)
    <|> exprAnswer
    <|> (do reserved lexer "true"; return $ Term $ fromBool True)
    <|> (do reserved lexer "false"; return $ Term $ fromBool False)

exprParens = do
  expr <- parens lexer exprParser
  u <- optionMaybe unitsTerm
  return $ case u of
    Nothing -> expr
    Just units -> Convert units expr

exprAnswer = do
  reserved lexer "_"
  u <- optionMaybe unitsTerm
  return $ case u of
    Nothing -> Answer
    Just units -> Convert units Answer

exprCast = do
  reservedOp lexer ":" <|> reserved lexer "to"
  unitsParser

exprApply = do
  script <- getState
  s <- identifier lexer
  xs <- sepBy exprParser (lexeme lexer $ char ';')
  case M.lookup s $ funcs script of
    Just f -> return $ Call f xs
    Nothing -> fail $ s ++ " ?"

exprTable =
  [ [prefix "-" negate, prefix "+" id],
    [binary "^" powScalar AssocLeft],
    [binary "*" (binOp (*)) AssocLeft, binary "/" (binOp (/)) AssocLeft],
    [binaryConv "+" (binOp (+)) AssocLeft, binaryConv "-" (binOp (-)) AssocLeft],
    [binaryConv "==" (cmpOp (==)) AssocLeft, binaryConv "/=" (cmpOp (/=)) AssocLeft, binaryConv "<" (cmpOp (<)) AssocLeft, binaryConv ">" (cmpOp (>)) AssocLeft, binaryConv "<=" (cmpOp (<=)) AssocLeft, binaryConv ">=" (cmpOp (>=)) AssocLeft],
    [Postfix (do Convert <$> exprCast)]
  ]
  where
    binOp f x y = Right $ f x y
    cmpOp f x y = Right $ fromBool (f x y)

prefix op f = Prefix (do reservedOp lexer op; return $ Unary f)

binary op f = Infix (do reservedOp lexer op; return $ Binary f)

binaryConv op f = Infix (do reservedOp lexer op; return $ BinaryConv f)
