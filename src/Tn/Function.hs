module Tn.Function where

import qualified Data.Vector as V
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
shiftArg x (Typed u) = convertUnits x u
shiftArg _ _ = throwError TypeMismatch

{-
defaultFunctions :: [(Symbol, Function)]
defaultFunctions =
  [ ("if", wrapFunc _if [Untyped, Any, Any]),
    -- num functions
    ("abs", unaryFunc abs Any),
    ("recip", unaryFunc recip Any),
    ("signum", unaryFunc signum Any),
    ("sqrt", unaryFunc (`powScalar` 0.5) Any),
    -- floating functions
    ("sin", unaryFunc (mapFloating sin) $ Typed $ Dims [(_angle, 1)]),
    ("cos", unaryFunc (mapFloating cos) $ Typed $ Dims [(_angle, 1)]),
    ("tan", unaryFunc (mapFloating tan . (Units [(_rad, 1)] `convertTo`)) $ Typed $ Dims [(_angle, 1)]),
    ("sinh", unaryFunc (mapFloating sinh) $ Typed $ Dims [(_angle, 1)]),
    ("cosh", unaryFunc (mapFloating cosh) $ Typed $ Dims [(_angle, 1)]),
    ("tanh", unaryFunc (mapFloating tanh) $ Typed $ Dims [(_angle, 1)]),
    ("asin", unaryFunc (mapFloating asin) Untyped),
    ("acos", unaryFunc (mapFloating acos) Untyped),
    ("atan", unaryFunc (mapFloating atan) Untyped),
    ("asinh", unaryFunc (mapFloating asinh) Untyped),
    ("acosh", unaryFunc (mapFloating acosh) Untyped),
    ("atanh", unaryFunc (mapFloating atanh) Untyped)
    -- realfrac functions
    -- ("ceil", unaryFunc (mapIntegral ceiling) Any),
    -- ("exp", unaryFunc (mapFloating exp) Untyped),
    -- ("floor", unaryFunc (mapFloating floor) Any),
    -- ("log", unaryFunc (mapFloating log) Untyped),
    -- ("round", unaryFunc (mapFloating round) Any),
    -- ("truncate", unaryFunc (mapFloating truncate) Any)
  ]
-}
{-
      ("pi", func _pi []),
    ]
  where
    dims xs = [Typed $ baseDims x | x <- xs]

-- unary function
unaryDef f = fmap $ fromReal . f . fromRational . toRational

radians = Units $ M.fromList [(_radian, 1)]

_if [test, t, e] = Right $ if test == 0 then e else t
_if _ = Left WrongArity

_abs [x] = unaryDef abs $ Right x
_abs _ = Left WrongArity

_signum [x] = unaryDef signum $ Right x
_signum _ = Left WrongArity

_sqrt [x] = Scalar 0 Nothing -- powScalar x 0.5
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

_pi [] = Right $ Scalar (toRational pi) Nothing
_pi _ = Left WrongArity

_sin [x] = unaryDef sin $ convertTo x radians
_sin _ = Left WrongArity

_cos [x] = unaryDef cos $ convertTo x radians
_cos _ = Left WrongArity

_tan [x] = unaryDef tan $ convertTo x radians
_tan _ = Left WrongArity

_sinh [x] = unaryDef sinh $ convertTo x radians
_sinh _ = Left WrongArity

_cosh [x] = unaryDef cosh $ convertTo x radians
_cosh _ = Left WrongArity

_tanh [x] = unaryDef tanh $ convertTo x radians
_tanh _ = Left WrongArity

_asin [x] = unaryDef asin (Right x) >>= (`convertTo` radians)
_asin _ = Left WrongArity

_acos [x] = unaryDef acos (Right x) >>= (`convertTo` radians)
_acos _ = Left WrongArity

_atan [x] = unaryDef atan (Right x) >>= (`convertTo` radians)
_atan _ = Left WrongArity

_asinh [x] = unaryDef asinh (Right x) >>= (`convertTo` radians)
_asinh _ = Left WrongArity

_acosh [x] = unaryDef acosh (Right x) >>= (`convertTo` radians)
_acosh _ = Left WrongArity

_atanh [x] = unaryDef atanh (Right x) >>= (`convertTo` radians)
_atanh _ = Left WrongArity

wrapFunc :: Function -> [Arg] -> Function
wrapFunc f args xs = zipWithM mapArg xs args >>= f
  where
    mapArg :: Scalar -> Arg -> Either EvalError Scalar
    mapArg x@(Scalar _ _) Any = Right x
    mapArg x@(Scalar _ Nothing) Untyped = Right x
    mapArg x@(Scalar _ (Just u)) (Typed d)
      | dims u == d = Right x
      | otherwise = Left InvalidArg
    mapArg _ _ = Left InvalidArg

unaryFunc :: (Scalar -> Scalar) -> Arg -> Function
unaryFunc f arg = wrapFunc func [arg]
  where
    func [x@(Scalar _ _)] = Right $ f x
    func [_] = Left InvalidArg
    func _ = Left ArityMismatch

binaryFunc :: (Scalar -> Scalar -> Scalar) -> Arg -> Arg -> Function
binaryFunc f xarg yarg = wrapFunc func [xarg, yarg]
  where
    func [x@(Scalar _ _), y@(Scalar _ _)] = Right $ f x y
    func [_, _] = Left InvalidArg
    func _ = Left ArityMismatch
-}
