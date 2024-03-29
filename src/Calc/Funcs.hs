{-# LANGUAGE OverloadedStrings #-}

module Calc.Funcs where

import Calc.Dims
import Calc.Error
import Calc.Scalar
import Calc.Units hiding (_pi)
import Control.Monad
import Data.Map.Strict as M

type Func = [Scalar] -> Either Error Scalar

data Arg = Any | Typed Dims

funcMap =
  M.fromList
    [ ("if", func _if [Any, Any, Any]),
      ("abs", func _abs [Any]),
      ("signum", func _signum [Any]),
      ("sqrt", func _sqrt [Any]),
      ("log", func _log [Typed mempty]),
      ("exp", func _exp [Typed mempty]),
      ("truncate", func _truncate [Any]),
      ("floor", func _floor [Any]),
      ("ceil", func _ceiling [Any]),
      ("round", func _round [Any]),
      ("pi", func _pi []),
      ("sin", func _sin $ dims [Angle]),
      ("cos", func _cos $ dims [Angle]),
      ("tan", func _tan $ dims [Angle]),
      ("sinh", func _sinh $ dims [Angle]),
      ("cosh", func _cosh $ dims [Angle]),
      ("tanh", func _tanh $ dims [Angle]),
      ("asin", func _asin [Typed mempty]),
      ("acos", func _acos [Typed mempty]),
      ("atan", func _atan [Typed mempty]),
      ("asinh", func _asinh [Typed mempty]),
      ("acosh", func _acosh [Typed mempty]),
      ("atanh", func _atanh [Typed mempty])
    ]
  where
    dims xs = [Typed $ baseDims x | x <- xs]

func :: ([Scalar] -> Either Error Scalar) -> [Arg] -> ([Scalar] -> Either Error Scalar)
func f args xs = zipWithM mapArg args xs >>= f
  where
    mapArg Any x = Right x
    mapArg (Typed dims) x@(Scalar _ d _) =
      if d == dims
        then Right x
        else Left $ WrongDims d dims

unaryDef f = fmap $ fromReal . f . fromRational . toRational

radians = Units $ M.fromList [(_radian, 1)]

_if [test, t, e] = Right $ if test == 0 then e else t
_if _ = Left WrongArity

_abs [x] = unaryDef abs $ Right x
_abs _ = Left WrongArity

_signum [x] = unaryDef signum $ Right x
_signum _ = Left WrongArity

_sqrt [x] = powScalar x 0.5
_sqrt _ = Left WrongArity

_log [x] = unaryDef log $ Right x
_log _ = Left WrongArity

_exp [x] = unaryDef exp $ Right x
_exp _ = Left WrongArity

_truncate [x] = unaryDef truncate $ Right x
_truncate _ = Left WrongArity

_floor [x] = unaryDef floor $ Right x
_floor _ = Left WrongArity

_ceiling [x] = unaryDef ceiling $ Right x
_ceiling _ = Left WrongArity

_round [x] = unaryDef round $ Right x
_round _ = Left WrongArity

_pi [] = Right $ fromReal pi
_pi _ = Left WrongArity

_sin [x] = unaryDef sin $ convert x radians
_sin _ = Left WrongArity

_cos [x] = unaryDef cos $ convert x radians
_cos _ = Left WrongArity

_tan [x] = unaryDef tan $ convert x radians
_tan _ = Left WrongArity

_sinh [x] = unaryDef sinh $ convert x radians
_sinh _ = Left WrongArity

_cosh [x] = unaryDef cosh $ convert x radians
_cosh _ = Left WrongArity

_tanh [x] = unaryDef tanh $ convert x radians
_tanh _ = Left WrongArity

_asin [x] = unaryDef asin (Right x) >>= (`convert` radians)
_asin _ = Left WrongArity

_acos [x] = unaryDef acos (Right x) >>= (`convert` radians)
_acos _ = Left WrongArity

_atan [x] = unaryDef atan (Right x) >>= (`convert` radians)
_atan _ = Left WrongArity

_asinh [x] = unaryDef asinh (Right x) >>= (`convert` radians)
_asinh _ = Left WrongArity

_acosh [x] = unaryDef acosh (Right x) >>= (`convert` radians)
_acosh _ = Left WrongArity

_atanh [x] = unaryDef atanh (Right x) >>= (`convert` radians)
_atanh _ = Left WrongArity
