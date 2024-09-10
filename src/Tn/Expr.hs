{-# LANGUAGE OverloadedRecordDot #-}

module Tn.Expr
  ( Expr (..),
    exprParser,
    exprShifts,
  )
where

import qualified Data.Map.Strict as M
import Text.Parsec
import Text.Parsec.Expr
import Text.Parsec.Token
import Tn.Context
import Tn.Function
import Tn.Ops
import Tn.Parser
import Tn.Scalar
import Tn.Scope
import Tn.Symbol
import Tn.Unit
import Prelude hiding (Infix, Prefix, (<|>))

data Expr
  = Ans
  | Shift
  | Term Scalar
  | Convert Units Expr
  | UnaryOp (Scalar -> ResultT Scalar) Expr
  | BinaryOp (Scalar -> Scalar -> ResultT Scalar) Expr Expr
  | Apply Function [Expr]

exprParser :: Parsec String Scope Expr
exprParser = buildExpressionParser exprTable exprTerm <|> (do eof; return Ans)

exprShifts :: Expr -> Bool
exprShifts Shift = True
exprShifts (Convert _ e) = exprShifts e
exprShifts (UnaryOp _ e) = exprShifts e
exprShifts (BinaryOp _ a b) = exprShifts a || exprShifts b
exprShifts (Apply _ xs) = any exprShifts xs
exprShifts _ = False

exprTerm :: Parsec String Scope Expr
exprTerm =
  exprParens
    <|> brackets lexer exprApply
    <|> (do reserved lexer "_"; return Shift)
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

exprConvert :: Parsec String Scope Units
exprConvert = lexeme lexer (char ':') >> unitsParser

exprTable :: OperatorTable String Scope Identity Expr
exprTable =
  [ [prefix "-" $ return . negate, prefix "+" return],
    [binary "^" (^%) AssocLeft],
    [binary "*" (*%) AssocLeft, binary "/" (/%) AssocLeft],
    [binary "+" (+%) AssocLeft, binary "-" (-%) AssocLeft],
    [binary "==" (==%) AssocLeft, binary "~=" (~=%) AssocLeft, binary "/=" (/=%) AssocLeft, binary "<" (<%) AssocLeft, binary ">" (>%) AssocLeft, binary "<=" (<=%) AssocLeft, binary ">=" (>=%) AssocLeft],
    [Postfix (do Convert <$> exprConvert)]
  ]

prefix :: String -> (Scalar -> ResultT Scalar) -> Operator String Scope Identity Expr
prefix op f = Prefix (do reservedOp lexer op; return $ UnaryOp f)

binary :: String -> (Scalar -> Scalar -> ResultT Scalar) -> Assoc -> Operator String Scope Identity Expr
binary op f = Infix (do reservedOp lexer op; return $ BinaryOp f)

exprApply :: Parsec String Scope Expr
exprApply = do
  funcName <- identifier lexer <&> intern
  scope <- getState

  -- lookup the function in the scope
  f <- case M.lookup funcName scope._functions of
    Nothing -> fail $ "unknown function: " ++ show funcName
    Just f -> return f

  -- get all the arguments
  sepBy exprParser (lexeme lexer $ char ';') <&> Apply f
