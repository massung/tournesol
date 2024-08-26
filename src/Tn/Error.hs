module Tn.Error where

import Text.Parsec (ParseError)
import Tn.Dims
import Tn.Units

data Error
  = NoExpr
  | NoFunction String
  | ExprError ParseError
  | ConversionError Units Units
  | WrongArity
  | WrongDims Dims Dims
  | IllegalExponent
  | ReadError ParseError
  | Error IOException
  | ArithError ArithException
  deriving (Eq)

instance Exception Error

instance Show Error where
  show NoExpr = "no expression"
  show (NoFunction f) = unwords ["no function:", f]
  show (ExprError e) = show e
  show (ConversionError from to) = unwords ["no conversion possible from", show from, "to", show to]
  show WrongArity = "wrong arity"
  show (WrongDims from to) = unwords ["wrong dimensions; got", show from, "expected", show to]
  show IllegalExponent = "illegal exponent"
  show (ReadError e) = show e
  show (Error e) = show e
  show (ArithError e) = show e
