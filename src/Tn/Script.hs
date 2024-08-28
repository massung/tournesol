{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

module Tn.Script (scriptParser) where

import qualified Data.Map.Strict as M
import Data.Symbol
import Text.Parsec hiding ((<|>))
import Text.Parsec.Token
import Tn.Conv
import Tn.Dims
import Tn.Expr
import Tn.Function
import Tn.Parser
import Tn.Scalar
import Tn.Scope
import Tn.Units

scriptParser :: Parsec String Scope ()
scriptParser = void $ scriptDecl `sepBy` lexeme lexer newline

scriptDecl :: Parsec String Scope ()
scriptDecl = dimDecl <|> unitDecl

dimDecl :: Parsec String Scope ()
dimDecl = dimBase <|> dimDerived

dimBase :: Parsec String Scope ()
dimBase = do
  scope <- getState
  pos <- getPosition

  -- dim base <name>
  reserved lexer "dim"
  reserved lexer "base"

  -- dimension name
  name <- identifier lexer <&> intern

  -- add to the scope or fail
  either fail putState $ declDim name (Fundamental name) (Just pos) scope

dimDerived :: Parsec String Scope ()
dimDerived = do
  scope <- getState
  pos <- getPosition

  -- dim <name> = <dims>
  reserved lexer "dim"
  name <- identifier lexer <&> intern

  -- fundamental dimensions
  reservedOp lexer "="
  derived <- dimsParser

  -- add to the scope or fail
  either fail putState $ declDim name (Derived name derived) (Just pos) scope

dimParser :: Parsec String Scope Dim
dimParser = do
  name <- identifier lexer <&> intern

  -- does this dimension exist?
  (getState <&> M.lookup name . _dims) >>= \case
    Nothing -> fail $ "unknown dimensions " ++ show name
    Just (dim, _) -> return dim

unitDecl :: Parsec String Scope ()
unitDecl = unitBase

unitBase :: Parsec String Scope ()
unitBase = do
  scope <- getState
  pos <- getPosition

  -- unit base [si|storage] <symbol> ["name"] = <dim>
  reserved lexer "unit"
  reserved lexer "base"
  si <- optionMaybe $ reserved lexer "si"
  sym <- identifier lexer
  name <- option sym docString

  -- fundamental dimensions
  reservedOp lexer "="
  dim <- dimParser

  -- TODO: check to make sure that BASE units don't already exist

  -- create the new base units
  let u =
        Unit
          { _symbol = intern sym,
            _name = name,
            _dim = dim,
            _conv = Base
          }
   in if isJust si
        then either fail putState $ declUnits (siUnits u) (Just pos) scope
        else either fail putState $ declUnit u (Just pos) scope

unitDerived :: Parsec String Scope ()
unitDerived = do
  scope <- getState
  pos <- getPosition

  -- unit <symbol> ["name"] = <scalar>
  reserved lexer "unit"
  sym <- identifier lexer
  name <- option sym docString

  -- fundamental dimensions
  reservedOp lexer "="
  (Scalar x (Just u')) <- scalarParser

  -- is there dimension for the resulting units?
  case find ((== dims u') . fundamentalDims) $ baseDims scope of
    Nothing -> fail "unknown dimensions for unit"
    Just d ->
      let u =
            Unit
              { _symbol = intern sym,
                _name = name,
                _dim = d,
                _conv = unitsConv u' <> Linear x
              }
       in either fail putState $ declUnit u (Just pos) scope

docString :: Parsec String Scope String
docString = stringLiteral lexer
