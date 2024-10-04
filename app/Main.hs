{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main (main) where

import qualified Streamly.Data.Fold.Prelude as Fold
import qualified Streamly.Data.Stream.Prelude as S
import System.Console.CmdArgs
import System.Console.Haskeline
import System.Console.Haskeline.IO
import System.Environment
import System.IO
import Text.Parsec
import Text.Parsec.Token
import Tn.Builtins
import Tn.Debug
import Tn.Lexer
import Tn.Parser
import Prelude hiding ((<|>))

data Opts = Opts
  { scriptFiles :: [String],
    precision :: Maybe Int,
    sciNotation :: Bool,
    delim :: Maybe String,
    noUnits :: Bool,
    exprString :: Maybe String
  }
  deriving (Data, Typeable, Show, Eq)

getOpts :: IO Opts
getOpts =
  cmdArgs
    $ Opts
      { scriptFiles = def &= explicit &= name "f" &= name "file" &= typ "FILE" &= help "Load Tournesol script file",
        precision = def &= explicit &= name "p" &= name "precision" &= typ "DIGITS" &= help "Precision digits to output, defaults to 2",
        sciNotation = def &= explicit &= name "g" &= name "sci-notation" &= help "Output using scientific notation",
        noUnits = def &= explicit &= name "n" &= name "no-units" &= help "Don't output units",
        delim = def &= explicit &= name "d" &= name "delimiter" &= typ "FS" &= help "Input stream delimiter, defaults to $FS",
        exprString = def &= args &= typ "EXPRESSION"
      }
    &= program "Tournesol"
    &= summary motd
    &= details
      [ "Examples:",
        "  tn '1+2'",
        "  tn '6 ft + 3 in : m'",
        "  tn '500 N * 10 ft : BTU'",
        "  tn '10 GB / 2 hr : MB/s'",
        "  tn '2 * (1500 cm)^2 : acre'",
        "  tn '30 W * 6 min : J'",
        "  tn '10 J / 2 ft : N'",
        "  tn '2 * [sin 45 deg]'",
        "  tn '_ hz * _ m : mph' < values.txt"
      ]
    &= noAtExpand

motd :: String
motd = printf "Tournesol v%d.%d.%d, (c) Jeffrey Massung" major minor patch
  where
    major = 0 :: Int
    minor = 8 :: Int
    patch = 0 :: Int

printFormat :: Opts -> Scalar -> String
printFormat opts (Scalar x _) = printf "%%0.%d%s" sigfigs notation
  where
    sigfigs :: Int
    sigfigs = if denominator x == 1 then 0 else fromMaybe 2 opts.precision

    notation :: String
    notation = if opts.sciNotation then "g" else "f"

printAns :: Opts -> Scalar -> IO ()
printAns opts ans@(Scalar _ u) = do
  if isJust u && not opts.noUnits
    then printf (printFormat opts ans ++ " %U\n") ans ans
    else printf (printFormat opts ans ++ "\n") ans

runParserIO :: Parsec String Scope a -> Scope -> String -> IO a
runParserIO parser scope s = either (throw . SyntaxError) return parseResult
  where
    parseResult = runParser parser scope "" s

parseInputLine :: Scope -> String -> IO [Scalar]
parseInputLine scope line = do
  fs <- lookupEnv "FS" <&> fromMaybe ","
  runParserIO (scalars fs <|> fail "invalid value") scope line
  where
    scalars fs = do
      vals <- sepBy1 (whiteSpace lexer >> scalarParser) $ string fs
      eof
      return vals

parseExpr :: Scope -> String -> IO Expr
parseExpr = runParserIO $ do
  expr <- exprParser
  eof
  return expr

parseStatementOrExpr :: Scope -> String -> IO (Either Scope Expr)
parseStatementOrExpr = runParserIO statementOrExpr
  where
    statementOrExpr = do cmd <- statement <|> expr; eof; return cmd
    statement = statementParser >> getState <&> Left
    expr = exprParser <&> Right

runExpr :: Scope -> Expr -> [Scalar] -> IO Scalar
runExpr scope expr xs =
  case runWithContext (evalExpr expr) $ push xs (mkContext scope) of
    Left err -> throw err
    Right ans -> return ans

runCmd :: Opts -> Scope -> String -> IO Scope
runCmd opts scope s = do
  parseStatementOrExpr scope s >>= \case
    Left scope' -> return scope'
    Right expr -> do runExpr scope expr [] >>= printAns opts; return scope

prompt :: InputState -> IO String
prompt is = fromMaybe (fail "Bye!") <$> queryInput is (getInputLine ">> ")

contextHandler :: a -> (ContextError -> IO a)
contextHandler x err = do print err; return x

repl :: Opts -> InputState -> Scope -> IO ()
repl opts is scope = readEvalPrint >>= repl opts is
  where
    readEvalPrint = do
      line <- prompt is
      runCmd opts scope line `catch` contextHandler scope

runInteractive :: Opts -> Scope -> IO InputState -> IO ()
runInteractive opts scope is = do
  putStrLn motd
  bracketOnError is cancelInput $ flip (repl opts) scope

runInputStream :: Opts -> Scope -> String -> IO ()
runInputStream opts scope s = do
  expr <- parseExpr scope s
  if exprHasShift expr
    then processLoop expr
    else runExpr scope expr [] >>= printAns opts
  where
    processLoop :: Expr -> IO ()
    processLoop expr =
      S.unfoldrM readLine ()
        & S.filter (not . null)
        & S.mapM (parseInputLine scope)
        & S.mapM (runExpr scope expr)
        & S.mapM (printAns opts)
        & S.fold Fold.drain

    readLine :: () -> IO (Maybe (String, ()))
    readLine _ = do
      atEnd <- isEOF
      if atEnd
        then return Nothing
        else getLine >>= \line -> return $ Just (line, ())

loadScripts :: [String] -> IO Scope
loadScripts = foldM load mempty
  where
    load :: Scope -> String -> IO Scope
    load scope file =
      loadScriptFile file scope >>= \case
        Left err -> throw $ SyntaxError err
        Right scope' -> return scope'

main :: IO ()
main = do
  opts <- getOpts

  -- load script files or use the default script
  scope <- case opts.scriptFiles of
    [] -> return defaultScope
    files -> loadScripts files

  -- determine the run mode
  case opts.exprString of
    Nothing -> runInteractive opts scope $ initializeInput defaultSettings
    Just expr -> runInputStream opts scope expr
