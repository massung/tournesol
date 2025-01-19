{-# LANGUAGE OverloadedLists #-}

module Tn.Function where

import Tn.Context
import Tn.Dims
import Tn.Scalar
import Tn.Symbol
import Tn.Unit
import Prelude hiding (Any, Arg)

data Function = Function [ArgType] (ResultT Scalar)

data ArgType
  = Any
  | Untyped
  | UntypedInteger
  | Typed Units
  | TypedDims (Dims Symbol)

mapArgs :: [Scalar] -> [ArgType] -> ResultT [Scalar]
mapArgs [] [] = return []
mapArgs [] _ = throwError ArityMismatch
mapArgs _ [] = throwError ArityMismatch
mapArgs (x : xs) (arg : args) = do
  y <- shiftArg x arg
  mapArgs xs args <&> (y :)

shiftArg :: Scalar -> ArgType -> ResultT Scalar
shiftArg x Any = return x
shiftArg x@(Scalar _ Nothing) Untyped = return x
shiftArg x@(Scalar n Nothing) UntypedInteger =
  if snd (properFraction n :: (Integer, Double)) == 0
    then return x
    else throwError TypeMismatch
shiftArg x (Typed u) = convertToUnits x u
shiftArg x (TypedDims dims) =
  if maybe True ((== dims) . baseDims) (scalarUnits x)
    then return x
    else throwError TypeMismatch
shiftArg _ _ = throwError TypeMismatch
