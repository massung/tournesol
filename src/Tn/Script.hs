{-# LANGUAGE LambdaCase #-}

module Tn.Script
  ( loadScript,
    loadScriptFile,
    scriptParser,
  )
where

import qualified Data.Map.Strict as M
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
import Tn.Symbol
import Tn.System
import Tn.Unit

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
scriptDecl = do reserved lexer "dim"; dimDecl <|> unitDecl

-- dim [base] <name>
dimDecl :: Parsec String Scope ()
dimDecl = do
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

  -- base, derived, or conversion
  unitBase name <|> unitDerived name <|> unitConv name

unitBase :: Symbol -> Parsec String Scope ()
unitBase name = do
  reserved lexer "base"

  -- optional system and registered dimension
  system <- optionMaybe systemName
  base <- baseDim
  return ()

-- TODO: register the unit

unitDerived :: Symbol -> Parsec String Scope ()
unitDerived name = do
  units <- unitsParser
  return ()

-- TODO: register the unit

unitConv :: Symbol -> Parsec String Scope ()
unitConv name = do
  r <- scalarParser
  return ()

-- TODO: verify no compound units!

-- TODO: create the linear conversion (r units positive or negative?)

-- TODO: register the unit

systemName :: Parsec String Scope System
systemName = do
  (reserved lexer "imperial" >> return Imperial)
    <|> (reserved lexer "metric" >> return Metric)
    <|> (reserved lexer "storage" >> return Storage)

baseDim :: Parsec String Scope Base
baseDim = do
  dim <- identifier lexer <&> intern
  base <- getState <&> M.lookup dim . _dims

  -- ensure the dimension is registered
  maybe (fail "unknown dimension") return base
