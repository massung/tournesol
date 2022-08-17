module Calc.Parser.Script where

import Calc.Conv
import Calc.Dims
import Calc.Funcs
import Calc.Parser.Dims
import Calc.Parser.Expr
import Calc.Parser.Lexer
import Calc.Parser.Scalar
import Calc.Scalar
import Calc.Script
import Calc.Units
import Data.Map.Strict as M
import Text.Parsec
import Text.Parsec.Token hiding (symbol)

scriptParser = do
  whiteSpace lexer
  skipMany (scriptSystem <|> scriptFunction)
  eof
  getState

scriptAssign valueParser = do
  reservedOp lexer "="
  valueParser

scriptSystem = do
  reserved lexer "system"
  sys <- stringLiteral lexer <|> identifier lexer
  units <- many scriptUnits
  updateState $ \st -> st {systems=M.insert sys (fromList units) $ systems st}

scriptUnits = do
  reserved lexer "units"
  r <- option 1 rationalParser
  name <- identifier lexer
  Scalar x d u <- scriptAssign (scalarParser <|> scalarSingleton)
  case fromDims d of
    Nothing -> fail "unknown dimensions"
    Just dim -> let unit = Unit {dim=dim, symbol=name, conv=unitsConv u <> Linear (r / x)}
                 in return (name, unit)

scriptFunction = do
  reserved lexer "function"
  name <- identifier lexer
  args <- scriptArgs
  expr <- scriptAssign exprParser
  let f = scriptFunc args
   in updateState $ \st -> st {funcs = M.insert name (f expr) $ funcs st}

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
