module Tn.Error where

data NumericalError
  = DivByZero
  | DisparateUnits
  | IllegalExponent

instance Show NumericalError where
  show DivByZero = "division by zero"
  show DisparateUnits = "disparate units"
  show IllegalExponent = "illegal exponent"

data EvalError
  = ArityMismatch
  | DimensionsMismatch
  deriving (Eq)

instance Show EvalError where
  show ArityMismatch = "arity mismatch"
  show DimensionsMismatch = "disparate dimensions"

instance Exception NumericalError

instance Exception EvalError
