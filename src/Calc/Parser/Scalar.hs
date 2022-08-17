module Calc.Parser.Scalar where

import Calc.Units
import Calc.Parser.Lexer
import Calc.Parser.Units
import Calc.Scalar
import Text.Parsec
import Text.Parsec.Token

scalarParser = do
  n <- naturalOrFloat lexer
  u <- option mempty $ try unitsParser <|> unitsTerm
  return $ case n of
    Left i -> Scalar (fromIntegral i) (dims u) u
    Right f -> Scalar (toRational f) (dims u) u

scalarSingleton = fromUnits <$> unitsTerm
