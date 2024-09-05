{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

module Tn.Script
  ( loadScript,
    loadScriptFile,
  )
where

import qualified Algebra.Graph.Labelled.AdjacencyMap as G
import qualified Data.Map.Strict as M
import Text.Parsec
import Text.Parsec.Token
import Tn.Conv
import Tn.Dims
import Tn.Eval
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

scriptParser :: Parsec String Scope Scope
scriptParser = do
  whiteSpace lexer
  void $ scriptDecl `sepEndBy` lexeme lexer (lexeme lexer $ char ';')
  eof
  getState

scriptDecl :: Parsec String Scope ()
scriptDecl = dimDecl <|> unitDecl <|> systemUnits

-- dim [base] <name>
dimDecl :: Parsec String Scope ()
dimDecl = do
  reserved lexer "dim"

  -- fundamental dimension name
  name <- identifier lexer <&> intern

  -- register the base dimension in the scope
  getState <&> declDim name >>= \case
    Left err -> fail err
    Right scope -> putState scope

unitDecl :: Parsec String Scope ()
unitDecl = do
  reserved lexer "unit"

  -- unit being defined
  name <- identifier lexer <&> intern
  reservedOp lexer "="

  -- base, alias (derived), or conversion
  unitBase name <|> unitAlias name <|> unitConv name

unitBase :: Symbol -> Parsec String Scope ()
unitBase name = do
  reserved lexer "base"

  -- dimension
  base <- baseDim
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
  expr <- exprParser
  scope <- getState

  -- evaluate the expression
  x <- either fail return $ evalExpr (0, scope) expr

  -- extract the linear ratio, units, and exponent
  (r, (to, e)) <- case x of
    Scalar r (Just [(to, e)]) -> return (r, (to, e))
    _ -> fail "invalid conversion"

  -- create the unit and conversion graph
  let u = Unit name $ Derived [(to, abs e)]
      g =
        if e > 0
          then linearConvs u (to, 1 % toInteger e) $ recip r
          else linearConvs u (to, toInteger e % 1) r

  -- add the unit and conversions to the scope
  either fail (putState . declConvs g) $ declUnit u scope

baseDim :: Parsec String Scope Base
baseDim = do
  dim <- identifier lexer <&> intern
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
