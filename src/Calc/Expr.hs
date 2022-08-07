module Calc.Expr where

import Calc.Error
import Calc.Funcs
import Calc.Scalar
import Calc.Units

data Expr
  = Answer
  | Term Scalar
  | Convert Units Expr
  | Call Func [Expr]
  | Unary (Scalar -> Scalar) Expr
  | Binary (Scalar -> Scalar -> Either Error Scalar) Expr Expr
  | BinaryConv (Scalar -> Scalar -> Either Error Scalar) Expr Expr

hasPlaceholder Answer = True
hasPlaceholder (Term _) = False
hasPlaceholder (Convert _ x) = hasPlaceholder x
hasPlaceholder (Unary _ x) = hasPlaceholder x
hasPlaceholder (Binary _ x y) = hasPlaceholder x || hasPlaceholder y
hasPlaceholder (BinaryConv _ x y) = hasPlaceholder x || hasPlaceholder y
hasPlaceholder (Call _ xs) = any hasPlaceholder xs
