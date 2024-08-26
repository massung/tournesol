module Tn.Eval where

import Text.Parsec hiding (State)
import Tn.Error
import Tn.Expr
import Tn.Parser
import Tn.Scalar
import Tn.Script
import Tn.Units

type EvalResultT = ExceptT Error (State [Scalar])

eval :: Script -> [Scalar] -> String -> Either Error Scalar
eval script st s = case runParser exprParser script "" s of
  Left err -> throwError $ ReadError err
  Right expr -> evalState (runExceptT $ evalExpr expr) st

evalExpr :: Expr -> EvalResultT Scalar
evalExpr (Term x) = return x
evalExpr (Convert to x) = evalConvert x to
evalExpr (Unary f x) = evalUnary f x
evalExpr (Binary f x y) = evalBinary f x y

-- evalExpr (Apply f xs) = evalApply f xs

evalConvert :: Expr -> Units -> EvalResultT Scalar
evalConvert x to = do
  x' <- evalExpr x
  either throwError return $ convertTo x' to

evalUnary :: (Scalar -> Either Error Scalar) -> Expr -> EvalResultT Scalar
evalUnary f x = do
  x' <- evalExpr x
  either throwError return $ f x'

evalBinary :: (Scalar -> Scalar -> Either Error Scalar) -> Expr -> Expr -> EvalResultT Scalar
evalBinary f x y = do
  x' <- evalExpr x
  y' <- evalExpr y
  either throwError return $ f x' y'

-- evalApply :: Function -> [Expr] -> EvalResultT Scalar
-- evalApply f xs = do
--   xs' <- sequence [evalExpr x | x <- xs]
--   either throwError return $ f xs'
