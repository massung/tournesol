{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

module Tn.Script
  ( loadScript,
    loadScriptFile,
    statementParser,
  )
where

import qualified Algebra.Graph.Labelled.AdjacencyMap as G
import qualified Data.Map.Strict as M
import Text.Parsec
import Text.Parsec.Token
import Text.Printf
import Tn.Context
import Tn.Conv
import Tn.Eval
import Tn.Function
import Tn.Parser
import Tn.Scalar
import Tn.Scope
import Tn.Symbol
import Tn.System
import Tn.Unit
import Prelude hiding ((<|>))

loadScript :: String -> String -> Scope -> Either ParseError Scope
loadScript filename source scope = runParser scriptParser scope filename source

loadScriptFile :: String -> Scope -> IO (Either ParseError Scope)
loadScriptFile filename scope = do
  source <- readFile filename
  return $ loadScript filename source scope

comptimeExpr :: Parsec String Scope Scalar
comptimeExpr = do
  expr <- exprParser
  scope <- getState

  -- run the expression and return the result or error message
  either (fail . show) return $ runWithContext (evalExpr expr) (mkContext scope)

scriptParser :: Parsec String Scope Scope
scriptParser = do
  whiteSpace lexer
  statements
  eof
  getState

statements :: Parsec String Scope ()
statements = void $ statementParser `sepEndBy` end
  where
    end = lexeme lexer $ char ';' <|> newline

statementParser :: Parsec String Scope ()
statementParser =
  dimDecl
    <|> constDecl
    <|> unitDecl
    <|> systemUnits

-- dim [base] <name>
dimDecl :: Parsec String Scope ()
dimDecl = do
  reserved lexer "dim"

  -- fundamental dimension name
  name <- identifier lexer <&> intern . ("[" ++) . (++ "]")

  -- register the base dimension in the scope
  getState <&> declDim name >>= \case
    Left err -> fail err
    Right scope -> putState scope

-- const <name> = <scalar>
constDecl :: Parsec String Scope ()
constDecl = do
  reserved lexer "const"

  -- constant name
  name <- identifier lexer <&> intern
  reservedOp lexer "="

  -- parse the value
  x <- comptimeExpr
  scope <- getState

  -- all constants are just functions with no arguments
  either fail putState $ declFunction name (Function [] $ return x) scope

-- unit <name>
unitDecl :: Parsec String Scope ()
unitDecl = do
  reserved lexer "unit"

  -- unit being defined
  name <- identifier lexer <&> intern
  scope <- getState

  -- either `= ...` or it's a base dimension of itself
  do reservedOp lexer "=" >> (unitBase name <|> unitAlias name <|> unitConv name)
    <|> let dim = intern $ printf "[%s]" (show name)
            scope' = declDim dim scope >>= declUnit (Unit name $ Base dim)
         in either fail putState scope'

unitBase :: Symbol -> Parsec String Scope ()
unitBase name = do
  reserved lexer "base"

  -- dimension
  base <- parseDim
  scope <- getState

  -- add the unit and conversions to the scope
  either fail putState $ declUnit (Unit name base) scope

unitAlias :: Symbol -> Parsec String Scope ()
unitAlias name = do
  u <- unitsParser <&> Unit name . Derived 1
  scope <- getState

  -- register the unit in the scope
  either fail putState $ declUnit u scope

unitConv :: Symbol -> Parsec String Scope ()
unitConv name = do
  Scalar r units <- comptimeExpr
  scope <- getState

  -- A "simple" unit conversion is one that converts from a
  -- one dimension to the same dimension (e.g., time -> time).

  -- If this is a "simple" units conversion (e.g., ft -> m)
  -- then create a new Unit with the same Base dimensions as
  -- the unit being converted to.
  --
  -- If this is a "compound" conversion (e.g., L -> cm^3 or
  -- BTU -> J), then create a Derived unit with no conversion.

  (u, g) <- case units of
    Just [(to@(Unit _ (Base dim)), 1)] -> return $ makeSimpleConvTo to (Base dim) r
    Just to -> return $ makeDerivedConvTo to r
    _ -> fail "invalid units"

  -- add to the scope
  either fail (putState . declConvs g) $ declUnit u scope
  where
    makeSimpleConvTo :: Unit -> Base -> Rational -> (Unit, ConvGraph)
    makeSimpleConvTo to base r = let u = Unit name base in (u, linearConvs u to $ recip r)

    makeDerivedConvTo :: Units -> Rational -> (Unit, ConvGraph)
    makeDerivedConvTo to r = (Unit name $ Derived r to, G.empty)

parseDim :: Parsec String Scope Base
parseDim = do
  dim <- identifier lexer <&> intern . ("[" ++) . (++ "]")
  base <- getState <&> M.lookup dim . _dims

  -- ensure the dimension is registered
  maybe (fail "unknown dimension") return base

systemUnits :: Parsec String Scope ()
systemUnits = do
  system <- systemName
  name <- identifier lexer <&> intern
  scope <- getState

  -- lookup the unit in the scope
  u <- maybe (fail "unknown unit") return $ M.lookup name scope._units

  -- create the conversion graph
  let g = case system of
        Metric -> siConvs u
        Binary -> binaryConvs u
        _ -> G.vertex u

  -- register the graph
  putState $ declConvs g scope

systemName :: Parsec String Scope System
systemName = do
  (reserved lexer "imperial" >> return Imperial)
    <|> (reserved lexer "english" >> return Imperial)
    <|> (reserved lexer "si" >> return Metric)
    <|> (reserved lexer "binary" >> return Binary)
