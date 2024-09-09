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
    lookupDefaultUnit,
    lookupDefaultConv,
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

evalWithScope :: String -> Scalar -> Scope -> Either String Scalar
evalWithScope s ans scope =
  let ctx = mkContext scope._convs ans
   in case runParser exprParser scope "" s of
        Left err -> Left $ showParseError err
        Right expr -> mapLeft show $ runWithContext (evalExpr expr) ctx

evalWithDefaultScope :: String -> Either String Scalar
evalWithDefaultScope s = evalWithScope s 0 defaultScope

lookupDefaultUnit :: Symbol -> Maybe Unit
lookupDefaultUnit s = M.lookup s defaultScope._units

lookupDefaultConv :: (Symbol, Int) -> Symbol -> Maybe Conv
lookupDefaultConv (from, n) to = do
  from' <- lookupDefaultUnit from
  to' <- lookupDefaultUnit to
  findConv (from', n) to' defaultScope._convs

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
