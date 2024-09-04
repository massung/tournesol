module Tn.Eval
  ( module Tn.Expr,
    evalExpr,
  )
where

import Tn.Expr
import Tn.Function
import Tn.Ops
import Tn.Scalar
import Tn.Scope
import Tn.Unit

type EvalResultT = ExceptT String (State (Scalar, Scope))

evalExpr :: (Scalar, Scope) -> Expr -> Either String Scalar
evalExpr st expr = evalState (runExceptT $ evalExprTerm expr) st

evalExprTerm :: Expr -> EvalResultT Scalar
evalExprTerm Ans = get <&> fst
evalExprTerm (Term x) = return x
evalExprTerm (Convert to x) = evalConvert x to
evalExprTerm (UnaryOp f x) = evalUnaryOp f x
evalExprTerm (BinaryOp f x y) = evalBinaryOp f x y

-- evalExprTerm (Apply f xs) = evalApply f xs

evalConvert :: Expr -> Units -> EvalResultT Scalar
evalConvert term units = do
  x <- evalExprTerm term
  scope <- get <&> snd

  -- attempt to convert
  let ans = runWithScope scope $ convertUnits x units
   in either throwError return ans

evalUnaryOp :: (Scalar -> OpResultT Scalar) -> Expr -> EvalResultT Scalar
evalUnaryOp f term = do
  x <- evalExprTerm term
  scope <- get <&> snd

  -- attempt the operation
  let ans = runWithScope scope $ f x
   in either throwError return ans

evalBinaryOp :: (Scalar -> Scalar -> OpResultT Scalar) -> Expr -> Expr -> EvalResultT Scalar
evalBinaryOp f x y = do
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
