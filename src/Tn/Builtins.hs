{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Tn.Builtins where

import Data.FileEmbed
import Tn.Context
import Tn.Function
import Tn.Ops
import Tn.Scalar
import Tn.Scope
import Tn.Script
import Tn.Symbol
import Tn.Unit
import Prelude hiding (Any)

defaultScript :: String
defaultScript = $(embedStringFile "scripts/default.tn")

electricalScript :: String
electricalScript = $(embedStringFile "scripts/electrical.tn")

imperialScript :: String
imperialScript = $(embedStringFile "scripts/imperial.tn")

astronomicalScript :: String
astronomicalScript = $(embedStringFile "scripts/astronomical.tn")

maritimeScript :: String
maritimeScript = $(embedStringFile "scripts/maritime.tn")

storageScript :: String
storageScript = $(embedStringFile "scripts/storage.tn")

defaultFunctions :: Map Symbol Function
defaultFunctions =
  [ ("if", Function [Any, Any, Any] _if),
    ("negate", Function [Any] $ getLocal 0 <&> negate),
    ("abs", Function [Any] $ getLocal 0 <&> abs),
    ("recip", Function [Any] $ getLocal 0 <&> recip),
    ("sqrt", Function [Any] $ getLocal 0 >>= sqrtScalar),
    ("ceil", Function [Any] $ getLocal 0 <&> mapRealFrac ceiling),
    ("floor", Function [Any] $ getLocal 0 <&> mapRealFrac floor),
    ("round", Function [Any] $ getLocal 0 <&> mapRealFrac round),
    ("truncate", Function [Any] $ getLocal 0 <&> mapRealFrac truncate),
    ("exp", Function [Any] $ getLocal 0 <&> mapFloating exp),
    ("log", Function [Any] $ getLocal 0 <&> mapFloating log),
    ("sin", Function [Typed rads] $ getLocal 0 <&> mapFloating sin),
    ("cos", Function [Typed rads] $ getLocal 0 <&> mapFloating cos),
    ("tan", Function [Typed rads] $ getLocal 0 <&> mapFloating tan),
    ("sinh", Function [Typed rads] $ getLocal 0 <&> mapFloating sinh),
    ("cosh", Function [Typed rads] $ getLocal 0 <&> mapFloating cosh),
    ("tanh", Function [Typed rads] $ getLocal 0 <&> mapFloating tanh),
    ("asin", Function [Untyped] $ getLocal 0 <&> mapFloating asin),
    ("acos", Function [Untyped] $ getLocal 0 <&> mapFloating acos),
    ("atan", Function [Untyped] $ getLocal 0 <&> mapFloating atan),
    ("asinh", Function [Untyped] $ getLocal 0 <&> mapFloating asinh),
    ("acosh", Function [Untyped] $ getLocal 0 <&> mapFloating acosh),
    ("atanh", Function [Untyped] $ getLocal 0 <&> mapFloating atanh)
  ]
  where
    rads :: Units
    rads = [(Unit "rad" $ Base "[angle]", 1)]

defaultScope :: Scope
defaultScope =
  case foldr load (Right initialScope) scripts of
    Left err -> error err
    Right scope -> scope
  where
    initialScope :: Scope
    initialScope = mempty {_functions = defaultFunctions}

    load :: (String, String) -> Either String Scope -> Either String Scope
    load (fn, s) = either Left (loadScript fn s)

    scripts :: [(String, String)]
    scripts =
      [ ("storage.tn", storageScript),
        ("astronomical.tn", astronomicalScript),
        ("maritime.tn", maritimeScript),
        ("imperial.tn", imperialScript),
        ("electrical.tn", electricalScript),
        ("default.tn", defaultScript)
      ]

defaultContext :: Context
defaultContext = Context defaultScope._convs []

_if :: ResultT Scalar
_if =
  getLocal 0 >>= \case
    0 -> getLocal 2
    _ -> getLocal 1

-- cToF x = x * (9 % 5) + 32
-- fToC x = (x - 32) * (5 % 9)

-- cToK x = x + 273.15
-- kToC x = x - 273.15

-- cToR x = x * (9 % 5) + 491.67
-- rToC x = (x - 491.67) * (5 % 9)
