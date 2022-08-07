{-# LANGUAGE NegativeLiterals #-}
{-# LANGUAGE OverloadedStrings #-}

module Calc.Parser
  ( Calc.Parser.Dims,
    Calc.Parser.Expr,
    Calc.Parser.Lexer,
    Calc.Parser.Scalar,
    Calc.Parser.Script,
    Calc.Parser.Units
  ) where

import Calc.Parser.Dims
import Calc.Parser.Expr
import Calc.Parser.Lexer
import Calc.Parser.Scalar
import Calc.Parser.Script
import Calc.Parser.Units
import Calc.Scalar
import Calc.Script
import Calc.Units
import Data.Either.Extra
import Data.String
import Text.Parsec
import Text.Parsec.Expr
import Text.Parsec.Token

instance IsString Units where
  fromString s = fromRight (error "illegal units") $ parseUnits s
    where
      parseUnits :: String -> Either ParseError Units
      parseUnits = runParserT unitsParser defaultScript ""

instance IsString Scalar where
  fromString s = fromRight (error "illegal scalar") $ parseScalar s
    where
      parseScalar :: String -> Either ParseError Scalar
      parseScalar = runParserT scalarParser defaultScript ""

-- parseScalar = runParserT parser ""
--   where
--     parser = do
--       s <- unitsSign
--       n <- scalarParser
--       eof
--       if s < 0
--         then return $ negate n
--         else return n

-- parseUnits units = runParserT parser units ""
--   where
--     parser = do
--       u <- unitsParser
--       eof
--       return u
