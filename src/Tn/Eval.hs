{-# LANGUAGE OverloadedRecordDot #-}

module Tn.Eval
  ( module Tn.Expr,
    evalExpr,
  )
where

import Tn.Context
import Tn.Expr
import Tn.Function
import Tn.Scalar
import Tn.Unit
import Prelude hiding (Any)

evalExpr :: Expr -> ResultT Scalar
evalExpr Ans = getLocal 0
evalExpr Shift = shiftLocal
evalExpr (Term x) = return x
evalExpr (Convert to x) = evalConvert x to
evalExpr (UnaryOp f x) = evalUnaryOp f x
evalExpr (BinaryOp f x y) = evalBinaryOp f x y
evalExpr (Apply f xs) = evalApply f xs

evalConvert :: Expr -> Units -> ResultT Scalar
evalConvert x units = do
  x' <- evalExpr x
  convertUnits x' units

evalUnaryOp :: (Scalar -> ResultT Scalar) -> Expr -> ResultT Scalar
evalUnaryOp f x = evalExpr x >>= f

evalBinaryOp :: (Scalar -> Scalar -> ResultT Scalar) -> Expr -> Expr -> ResultT Scalar
evalBinaryOp f x y = do
  x' <- evalExpr x
  y' <- evalExpr y
  f x' y'

evalApply :: Function -> [Expr] -> ResultT Scalar
evalApply f xs = do
  xs' <- sequence [evalExpr x | x <- xs]
  args <- mapArgs xs' f._args

  -- push the arguments onto a new context and evaluate
  ans <- get <&> runWithContext f._body . push args

  -- return the answer in the current context
  either throwError return ans
