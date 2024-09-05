{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Tn.Debug
  ( module Tn.Builtins,
    module Tn.Conv,
    module Tn.Dims,
    module Tn.Eval,
    module Tn.Expr,
    module Tn.Ops,
    module Tn.Scalar,
    module Tn.Scope,
    module Tn.Script,
    module Tn.Unit,
    evalWithScope,
    evalWithDefaultScope,
  )
where

import Text.Parsec
import Tn.Builtins
import Tn.Conv
import Tn.Dims
import Tn.Eval
import Tn.Expr
import Tn.Ops
import Tn.Parser
import Tn.Scalar
import Tn.Scope
import Tn.Script
import Tn.Unit

evalWithScope :: Scope -> String -> Either String Scalar
evalWithScope scope s =
  case runParser exprParser scope "" s of
    Left err -> Left $ show err
    Right expr -> evalExpr (0, scope) expr

evalWithDefaultScope :: String -> Either String Scalar
evalWithDefaultScope = evalWithScope defaultScope

instance IsString Scalar where
  fromString s = case runParser scalarParser defaultScope "" s of
    Left _ -> error "Invalid scalar"
    Right x -> x

instance IsString (Dims Unit) where
  fromString s = case runParser unitsParser defaultScope "" s of
    Left _ -> error "Invalid units"
    Right u -> u
