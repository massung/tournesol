module Tn.Expr where

import Tn.Error
import Tn.Function
import Tn.Scalar
import Tn.Units

data Expr
  = Ans
  | Term Scalar
  | Convert Units Expr
  | Unary (Scalar -> Either EvalError Scalar) Expr
  | Binary (Scalar -> Scalar -> Either EvalError Scalar) Expr Expr
  | Apply Function [Expr]
