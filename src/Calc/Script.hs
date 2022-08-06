{-# LANGUAGE TemplateHaskell #-}

module Calc.Script where

import Calc.Dims
import Calc.Error
import Calc.Eval
import Calc.Expr
import Calc.Funcs
import Calc.Lexer
import Calc.Parser
import Calc.Units
import Control.Exception
import Control.Monad.Except
import Control.Monad.State.Strict
import Data.FileEmbed
import Data.Functor.Identity
import Data.Map.Strict as M
import Text.Parsec
import Text.Parsec.Token
import Calc.Scalar (Scalar)

type Funcs = Map String ([Scalar] -> Either Error Scalar)

builtInDefs :: IO Funcs
builtInDefs = either (throw . ExprError) return $ loadScriptContents defMap "built-ins.tn" source
  where
    source = $(embedStringFile "scripts/functions.tn")

scriptFunc :: [Arg] -> Expr -> Func
scriptFunc args expr = func (evalState $ runExceptT $ evalExpr expr) args

loadScripts defs [] = return defs
loadScripts defs (path : rest) = loadScript defs path >>= (`loadScripts` rest)

loadScript defs path = do
  contents <- readFile path
  loadScriptContents defs path contents

loadScriptContents :: Monad m => Funcs -> SourceName -> String -> m Funcs
loadScriptContents script path contents =
  case runParser scriptParser script path contents of
    Right script' -> return script'
    Left err -> throw $ ExprError err

scriptParser = do
  whiteSpace lexer
  skipMany (scriptUnits <|> scriptFunction)
  eof
  getState

scriptUnits = do
  reserved lexer "units"
  unit <- identifier lexer
  reservedOp lexer "="
  scalar <- scalarParser
  return ()

scriptFunction :: ParsecT String Funcs Identity ()
scriptFunction = do
  reserved lexer "function"
  def <- identifier lexer
  args <- scriptArgs
  reservedOp lexer "="
  expr <- exprParser
  let f = scriptFunc args
   in updateState $ M.insert def (f expr)

scriptArgs = brackets lexer (sepBy scriptArg $ lexeme lexer (char ';'))

scriptArg = do
  arg <- typedArg <|> anyArg <|> noneArg
  name <- optionMaybe $ do reservedOp lexer ":"; identifier lexer
  return arg
  where
    anyArg = do reserved lexer "any"; return Any
    noneArg = do reserved lexer "none"; return $ Typed mempty
    typedArg = do
      (dim, e) <- dimParser
      return $ Typed $ mapDims (* e) (baseDims dim)
