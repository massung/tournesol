{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

module Tn.Script
  ( loadScript,
    loadScriptFile,
    statementParser,
    showParseError,
  )
where

import qualified Algebra.Graph.Labelled.AdjacencyMap as G
import qualified Data.Map.Strict as M
import Text.Parsec
import Text.Parsec.Error
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

loadScript :: String -> String -> Scope -> Either String Scope
loadScript filename source scope = mapLeft showParseError scope'
  where
    scope' = runParser scriptParser scope filename source

loadScriptFile :: String -> Scope -> IO (Either String Scope)
loadScriptFile filename scope = do
  source <- readFile filename
  return $ loadScript filename source scope

showParseError :: ParseError -> String
showParseError err = printf "error on line %d of %s: %s" line file msg
  where
    pos = errorPos err
    file = sourceName pos
    line = sourceLine pos
    msg = case [e | (Message e) <- errorMessages err] of
      [s] -> s
      _ -> "syntax error"

comptimeEval :: Expr -> Parsec String Scope Scalar
comptimeEval expr = do
  scope <- getState

  -- create a temporary context from the current scope
  let ctx = mkContext scope._convs 0

  -- run the xpression and return the result or error message
  either (fail . show) return $ runWithContext (evalExpr expr) ctx

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
  x <- scalarParser
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
    <|> let scope' = declDim name scope >>= declUnit (Unit name $ Base name)
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
  u <- unitsParser <&> Unit name . Derived
  scope <- getState

  -- register the unit in the scope
  either fail putState $ declUnit u scope

unitConv :: Symbol -> Parsec String Scope ()
unitConv name = do
  x <- exprParser >>= comptimeEval
  scope <- getState

  -- extract the linear ratio, units, and exponent
  (r, (to, e)) <- case x of
    Scalar r (Just [(to, e)]) -> return (r, (to, e))
    _ -> fail "invalid conversion"

  -- create the unit and conversion graph
  let u = Unit name $ Derived [(to, abs e)]
      g =
        if e > 0
          then linearConvs u (to, 1 % toInteger e) $ recip r
          else linearConvs u (to, abs $ toInteger e % 1) r

  -- add the unit and conversions to the scope
  either fail (putState . declConvs g) $ declUnit u scope

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
