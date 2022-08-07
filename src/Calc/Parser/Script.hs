module Calc.Parser.Script where

import Calc.Dims
import Calc.Funcs
import Calc.Parser.Dims
import Calc.Parser.Expr
import Calc.Parser.Lexer
import Calc.Parser.Scalar
import Calc.Parser.Units
import Calc.Script
import Data.Map.Strict as M
import Text.Parsec
import Text.Parsec.Token

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

scriptFunction = do
  reserved lexer "function"
  def <- identifier lexer
  args <- scriptArgs
  reservedOp lexer "="
  expr <- exprParser
  let f = scriptFunc args
   in updateState $ \st -> st {funcs = M.insert def (f expr) $ funcs st}

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
