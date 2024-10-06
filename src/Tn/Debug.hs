{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Tn.Debug
  ( module Tn.Builtins,
    module Tn.Context,
    module Tn.Conv,
    module Tn.Dims,
    module Tn.Eval,
    module Tn.Expr,
    module Tn.Ops,
    module Tn.Scalar,
    module Tn.Scope,
    module Tn.Script,
    module Tn.Symbol,
    module Tn.Unit,
    evalWithScope,
    evalWithDefaultScope,
    runWithScope,
    runWithDefaultScope,
  )
where

import qualified Data.Map.Strict as M
import Text.Parsec
import Tn.Builtins
import Tn.Context
import Tn.Conv
import Tn.Dims
import Tn.Eval
import Tn.Expr
import Tn.Ops
import Tn.Parser
import Tn.Scalar
import Tn.Scope
import Tn.Script
import Tn.Symbol
import Tn.Unit

evalWithScope :: String -> Scope -> Either ContextError Scalar
evalWithScope s scope =
  case runParser exprParser scope "" s of
    Left err -> Left $ SyntaxError err
    Right expr -> runWithScope (evalExpr expr) scope

evalWithDefaultScope :: String -> Either ContextError Scalar
evalWithDefaultScope s = evalWithScope s defaultScope

runWithScope :: ResultT a -> Scope -> Either ContextError a
runWithScope r scope = runWithContext r $ mkContext scope

runWithDefaultScope :: ResultT a -> Either ContextError a
runWithDefaultScope r = runWithScope r defaultScope

instance IsString Scalar where
  fromString s = case runParser scalarParser defaultScope "" s of
    Left _ -> error "Invalid scalar"
    Right x -> x

instance IsString (Dims Unit) where
  fromString s = case runParser unitsParser defaultScope "" s of
    Left _ -> error "Invalid units"
    Right u -> u

instance IsString Unit where
  fromString s = case M.lookup (intern s) defaultScope._units of
    Nothing -> error "Invalid unit"
    Just u -> u
