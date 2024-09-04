{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedLists #-}

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
scriptDecl = dimDecl <|> unitDecl

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

  -- optional system and registered dimension
  system <- optionMaybe systemName
  base <- baseDim
  scope <- getState

  -- build the graph for the unit
  let u = Unit name base
      g = case system of
        Just Metric -> siConvs u
        Just Binary -> binaryConvs u
        _ -> G.vertex u

  -- add the unit and conversions to the scope
  either fail (putState . declConvs g) $ declUnit u scope

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
  r <- either fail return $ evalExpr (0, scope) expr

  -- the conversion should not consist of compound units
  (cu, e) <- case toList <$> scalarUnits r of
    Just [units] -> return units
    Nothing -> fail "unit cannot be derived from constant"
    _ -> fail "unit cannot be dervived from compound units"

  -- find the base dimensions of the units
  _base <- case baseDims [(cu, abs e)] of
    [(sym, _)] -> return $ Base sym
    _ -> fail "unit cannot be derived from compound dimensions"

  -- define the unit and conversion
  let u = Unit name $ Derived [(cu, abs e)] -- base
      g =
        if e > 0
          then linearConvs cu u $ toRational r
          else linearConvs u cu $ toRational r

  -- add the unit and conversions to the scope
  either fail (putState . declConvs g) $ declUnit u scope

systemName :: Parsec String Scope System
systemName = do
  (reserved lexer "imperial" >> return Imperial)
    <|> (reserved lexer "english" >> return Imperial)
    <|> (reserved lexer "si" >> return Metric)
    <|> (reserved lexer "binary" >> return Binary)

baseDim :: Parsec String Scope Base
baseDim = do
  dim <- identifier lexer <&> intern
  base <- getState <&> M.lookup dim . _dims

  -- ensure the dimension is registered
  maybe (fail "unknown dimension") return base
