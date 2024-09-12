module Tn.Function where

import Tn.Context
import Tn.Scalar
import Tn.Unit
import Prelude hiding (Any, Arg)

data Function = Function
  { _args :: [ArgType],
    _body :: ResultT Scalar
  }

data ArgType
  = Any
  | Untyped
  | UntypedInteger
  | Typed Units

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
shiftArg x@(Scalar i Nothing) UntypedInteger =
  if denominator i == 1
    then return x
    else throwError TypeMismatch
shiftArg x (Typed u) = convertUnits x u
shiftArg _ _ = throwError TypeMismatch
