{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

module Tn.Script
  ( loadScript,
    loadScriptFile,
    scriptParser,
  )
where

import qualified Data.Map.Strict as M
import Data.Symbol
import Text.Parsec hiding ((<|>))
import Text.Parsec.Token
import Tn.Conv
import Tn.Dims
import Tn.Eval
import Tn.Expr
import Tn.Function
import Tn.Parser
import Tn.Scalar
import Tn.Scope
import Tn.Units

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
dimDecl = reserved lexer "dim" >> dimBase <|> dimDerived

dimBase :: Parsec String Scope ()
dimBase = do
  reserved lexer "base"

  -- dimension name
  scope <- getState
  pos <- getPosition
  name <- identifier lexer <&> intern

  -- add to the scope or fail
  either fail putState $ declDim name (Fundamental name) (Just pos) scope

dimDerived :: Parsec String Scope ()
dimDerived = do
  name <- identifier lexer <&> intern
  scope <- getState
  pos <- getPosition

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
unitDecl = reserved lexer "unit" >> unitBase <|> unitAlias <|> unitDerived

unitBase :: Parsec String Scope ()
unitBase = do
  reserved lexer "base"

  -- unit base [si|storage] <symbol> ["name"] = <dim>
  scope <- getState
  pos <- getPosition
  si <- optionMaybe $ reserved lexer "si"
  sym <- identifier lexer
  name <- option sym docString

  -- fundamental dimensions
  reservedOp lexer "="
  dim <- dimParser

  -- check to make sure that base units don't already exist
  let org = elem dim [u._dim | (u, _) <- baseUnits scope]
   in when org $ fail "base units already exist for dimension"

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

unitAlias :: Parsec String Scope ()
unitAlias = do
  reserved lexer "alias"

  -- unit alias <symbol> ["name"] = <units>
  scope <- getState
  pos <- getPosition
  sym <- identifier lexer
  name <- option sym docString

  -- fundamental dimensions
  reservedOp lexer "="
  alias <- unitsParser

  -- is there a dimension for the resulting units?
  case find ((== dims alias) . fundamentalDims) $ baseDims scope of
    Nothing -> fail "unknown dimensions for unit"
    Just dim ->
      let u =
            Unit
              { _symbol = intern sym,
                _name = name,
                _dim = dim,
                _conv = unitsConv alias
              }
       in either fail putState $ declUnit u (Just pos) scope

unitDerived :: Parsec String Scope ()
unitDerived = do
  scope <- getState
  pos <- getPosition
  sym <- identifier lexer
  name <- option sym docString

  -- fundamental dimensions
  reservedOp lexer "="

  -- a derived unit is an expression
  (Scalar x units) <-
    exprParser >>= \case
      e@(Term _) -> either (fail . show) return $ evalExpr 0 e
      e@(Unary {}) -> either (fail . show) return $ evalExpr 0 e
      e@(Binary {}) -> either (fail . show) return $ evalExpr 0 e
      -- TODO: handle Apply f [exprs] -> create a Conv function pair?
      _ -> fail "illegal derived units expression"

  -- ensure the units exist, otherwise it's just a constant value
  u' <- maybe (fail "illegal derived units expression") (return . recipUnits) units

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
