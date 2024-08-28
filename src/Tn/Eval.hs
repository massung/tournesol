module Tn.Eval (evalExpr) where

import Tn.Error
import Tn.Expr
import Tn.Function
import Tn.Scalar
import Tn.Units

type EvalResultT = ExceptT EvalError (State Scalar)

evalExpr :: Scalar -> Expr -> Either EvalError Scalar
evalExpr ans expr = evalState (runExceptT $ evalExprTerm expr) ans

evalExprTerm :: Expr -> EvalResultT Scalar
evalExprTerm Ans = get
evalExprTerm (Term x) = return x
evalExprTerm (Convert to x) = evalConvert x to
evalExprTerm (Unary f x) = evalUnary f x
evalExprTerm (Binary f x y) = evalBinary f x y
evalExprTerm (Apply f xs) = evalApply f xs

evalConvert :: Expr -> Units -> EvalResultT Scalar
evalConvert x to = evalExprTerm x <&> convertTo to

evalUnary :: (Scalar -> Either EvalError Scalar) -> Expr -> EvalResultT Scalar
evalUnary f x = do
  x' <- evalExprTerm x
  either throwError return $ f x'

evalBinary :: (Scalar -> Scalar -> Either EvalError Scalar) -> Expr -> Expr -> EvalResultT Scalar
evalBinary f x y = do
  x' <- evalExprTerm x
  y' <- evalExprTerm y
  either throwError return $ f x' y'

evalApply :: Function -> [Expr] -> EvalResultT Scalar
evalApply f xs = do
  xs' <- sequence [evalExprTerm x | x <- xs]
  either throwError return $ f xs'
