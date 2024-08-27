module Tn.Eval where

import Tn.Error
import Tn.Expr
import Tn.Function
import Tn.Scalar
import Tn.Units

type EvalResultT = ExceptT EvalError (State Scalar)

eval :: Scalar -> Expr -> Either EvalError Scalar
eval ans expr = evalState (runExceptT $ evalExpr expr) ans

evalExpr :: Expr -> EvalResultT Scalar
evalExpr Ans = get
evalExpr (Term x) = return x
evalExpr (Convert to x) = evalConvert x to
evalExpr (Unary f x) = evalUnary f x
evalExpr (Binary f x y) = evalBinary f x y
evalExpr (Apply f xs) = evalApply f xs

evalConvert :: Expr -> Units -> EvalResultT Scalar
evalConvert x to = evalExpr x <&> convertTo to

evalUnary :: (Scalar -> Either EvalError Scalar) -> Expr -> EvalResultT Scalar
evalUnary f x = do
  x' <- evalExpr x
  either throwError return $ f x'

evalBinary :: (Scalar -> Scalar -> Either EvalError Scalar) -> Expr -> Expr -> EvalResultT Scalar
evalBinary f x y = do
  x' <- evalExpr x
  y' <- evalExpr y
  either throwError return $ f x' y'

evalApply :: Function -> [Expr] -> EvalResultT Scalar
evalApply f xs = do
  xs' <- sequence [evalExpr x | x <- xs]
  either throwError return $ f xs'
