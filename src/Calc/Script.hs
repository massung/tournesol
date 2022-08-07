{-# LANGUAGE TemplateHaskell #-}

module Calc.Script where

import Calc.Dims
import Calc.Error
import Calc.Eval
import Calc.Expr
import Calc.Funcs
import Calc.Scalar
import Calc.Units
import Control.Exception
import Control.Monad.Except
import Control.Monad.State.Strict
import Data.FileEmbed
import Data.Functor.Identity
import Data.Map.Strict as M
import Text.Parsec

data Script = Script
  { units :: Map String Unit,
    funcs :: Map String Func
  }

defaultScript =
  Script
    { units = unitMap,
      funcs = funcMap
    }

scriptFunc :: [Arg] -> Expr -> Func
scriptFunc args expr = func (evalState $ runExceptT $ evalExpr expr) args

{-
builtInScript :: IO Script
builtInScript = either (throw . ExprError) return $ loadScriptContents defaultScript "built-ins.tn" source
  where
    source = $(embedStringFile "scripts/functions.tn")

loadScripts defs [] = return defs
loadScripts defs (path : rest) = loadScript defs path >>= (`loadScripts` rest)

loadScript defs path = do
  contents <- readFile path
  loadScriptContents defs path contents

loadScriptContents :: Monad m => Script -> SourceName -> String -> m Script
loadScriptContents script path contents =
  case runParser scriptParser script path contents of
    Right script' -> return script'
    Left err -> throw $ ExprError err
-}
