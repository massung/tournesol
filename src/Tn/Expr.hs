module Tn.Expr where

import Tn.Error
import Tn.Function
import Tn.Scalar
import Tn.Units

data Expr
  = Term Scalar
  | Convert Units Expr
  | Unary (Scalar -> Either Error Scalar) Expr
  | Binary (Scalar -> Scalar -> Either Error Scalar) Expr Expr

--  | Apply Function [Expr]
