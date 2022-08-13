module Calc.Script where

import Calc.Eval
import Calc.Expr
import Calc.Funcs
import Calc.Units
import Control.Monad.Except
import Control.Monad.State.Strict
import Data.Map.Strict as M
import Data.Maybe

data Script = Script
  { units :: Map String Unit,
    systems :: Map String (Map String Unit),
    funcs :: Map String Func
  }

defaultScript =
  Script
    { units = unitMap,
      systems = mempty,
      funcs = funcMap
    }

scriptFunc :: [Arg] -> Expr -> Func
scriptFunc args expr = func (evalState $ runExceptT $ evalExpr expr) args

registerUnit :: String -> Unit -> Script -> Script
registerUnit sys unit script = script {systems=M.alter regTo sys (systems script)}
  where
    regTo Nothing = Just $ M.singleton sys unit
    regTo (Just units) = Just $ M.insert sys unit units

useSystem :: Script -> String -> Script
useSystem script sys = script {units = units script <> sysUnits}
  where
    sysUnits = fromMaybe mempty $ M.lookup sys (systems script)
