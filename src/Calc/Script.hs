module Calc.Script where

import Calc.Eval
import Calc.Expr
import Calc.Funcs
import Calc.Units
import Control.Monad.Except
import Control.Monad.State.Strict
import Data.Map.Strict as M

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
