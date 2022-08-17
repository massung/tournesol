{-# LANGUAGE TemplateHaskell #-}

module Calc.Base where

import Calc.Error
import Calc.Parser
import Calc.Script
import Control.Exception
import Control.Monad
import Data.FileEmbed
import Text.Parsec

baseScript :: IO Script
baseScript = foldM parseScript defaultScript [units, functions]
  where
    units = ("units.tn", $(embedStringFile "scripts/units.tn"))
    functions = ("funtions.tn", $(embedStringFile "scripts/functions.tn"))

loadScripts script [] = return script
loadScripts script (path : rest) = loadScript script path >>= (`loadScripts` rest)

loadScript :: Script -> FilePath -> IO Script
loadScript script path = do
  contents <- readFile path
  parseScript script (path, contents)

parseScript :: Monad m => Script -> (SourceName, String) -> m Script
parseScript script (path, contents) =
  case runParser scriptParser script path contents of
    Right script' -> return script'
    Left err -> throw $ ExprError err
