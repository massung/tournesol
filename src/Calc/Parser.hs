{-# LANGUAGE NegativeLiterals #-}
{-# LANGUAGE OverloadedStrings #-}

module Calc.Parser
  ( module Calc.Parser.Dims,
    module Calc.Parser.Expr,
    module Calc.Parser.Lexer,
    module Calc.Parser.Scalar,
    module Calc.Parser.Script,
    module Calc.Parser.Units,
    fromString,
    parseUnits,
    parseScalar
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

instance IsString Units where
  fromString = fromRight (error "illegal units") . runParser parser defaultScript ""
    where
      parser = do
        u <- unitsParser
        eof
        return u

instance IsString Scalar where
  fromString = fromRight (error "illegal scalar") . runParser parser defaultScript ""
    where
      parser = do
        s <- signParser
        n <- scalarParser
        eof
        if s < 0
          then return $ negate n
          else return n

-- common parsing helpers

parseUnits script = runParser unitsParser script ""

parseScalar script = runParser scalarParser script ""
