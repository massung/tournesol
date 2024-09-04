{-# LANGUAGE OverloadedRecordDot #-}

module Tn.Expr
  ( Expr (..),
    exprParser,
  )
where

import qualified Data.Map.Strict as M
import Text.Parsec
import Text.Parsec.Expr
import Text.Parsec.Token
import Tn.Error
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
  | Term Scalar
  | Convert Units Expr
  | UnaryOp (Scalar -> OpResultT Scalar) Expr
  | BinaryOp (Scalar -> Scalar -> OpResultT Scalar) Expr Expr

-- | Apply Function [Expr]
exprParser :: Parsec String Scope Expr
exprParser = buildExpressionParser exprTable exprTerm <|> (do eof; return Ans)

exprTerm :: Parsec String Scope Expr
exprTerm =
  exprParens
    -- <|> brackets lexer exprApply
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
exprConvert = lexeme lexer (char ':') >> unitsParser

exprTable :: OperatorTable String Scope Identity Expr
exprTable =
  [ [prefix "-" $ return . negate, prefix "+" return],
    [binary "^" (^%) AssocLeft],
    [binary "*" (*%) AssocLeft, binary "/" (/%) AssocLeft],
    [binary "+" (+%) AssocLeft, binary "-" (-%) AssocLeft],
    [binary "=" (==%) AssocLeft, binary "/=" (/=%) AssocLeft, binary "<" (<%) AssocLeft, binary ">" (>%) AssocLeft, binary "<=" (<=%) AssocLeft, binary ">=" (>=%) AssocLeft],
    [Postfix (do Convert <$> exprConvert)]
  ]

prefix :: String -> (Scalar -> OpResultT Scalar) -> Operator String Scope Identity Expr
prefix op f = Prefix (do reservedOp lexer op; return $ UnaryOp f)

binary :: String -> (Scalar -> Scalar -> OpResultT Scalar) -> Assoc -> Operator String Scope Identity Expr
binary op f = Infix (do reservedOp lexer op; return $ BinaryOp f)

-- exprApply :: ParsecT String Scope Identity Expr
-- exprApply = do
--   script <- getState
--   funcName <- identifier lexer <&> intern
--   xs <- sepBy exprParser (lexeme lexer $ char ';')
--   case M.lookup funcName script._funcs of
--     Just (f, _) -> return $ Apply f xs
--     Nothing -> fail "unknown function"
