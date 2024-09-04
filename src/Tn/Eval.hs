module Tn.Eval (evalExpr) where

import Tn.Error
import Tn.Expr
import Tn.Function
import Tn.Ops
import Tn.Scope
import Tn.Scalar
import Tn.Unit

type EvalResultT = ExceptT String (State (Scalar, Scope))

evalExpr :: (Scalar, Scope) -> Expr -> Either String Scalar
evalExpr st expr = evalState (runExceptT $ evalExprTerm expr) st

evalExprTerm :: Expr -> EvalResultT Scalar
evalExprTerm Ans = get <&> fst
evalExprTerm (Term x) = return x
evalExprTerm (Convert to x) = evalConvert x to
evalExprTerm (Unary f x) = evalUnary f x
evalExprTerm (Binary f x y) = evalBinary f x y
-- evalExprTerm (Apply f xs) = evalApply f xs

evalConvert :: Expr -> Units -> EvalResultT Scalar
evalConvert term units = do
  x <- evalExprTerm term
  scope <- get <&> snd

  -- attempt to convert
  let ans = runWithScope scope $ convertUnits x units
   in either throwError return ans

evalUnary :: (Scalar -> OpResultT Scalar) -> Expr -> EvalResultT Scalar
evalUnary f term = do
  x <- evalExprTerm term
  scope <- get <&> snd

  -- attempt the operation
  let ans = runWithScope scope $ f x
   in either throwError return ans

evalBinary :: (Scalar -> Scalar -> OpResultT Scalar) -> Expr -> Expr -> EvalResultT Scalar
evalBinary f x y = do
  x' <- evalExprTerm x
  y' <- evalExprTerm y

  -- get the conversion scope
  scope <- get <&> snd

  -- attempt the operation
  let ans = runWithScope scope $ f x' y'
   in either throwError return ans

-- evalApply :: Function -> [Expr] -> EvalResultT Scope
-- evalApply f xs = do
--   xs' <- sequence [evalExprTerm x | x <- xs]
--   either throwError return $ f xs'
