{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import System.Console.CmdArgs
import System.Console.Haskeline
import System.Console.Haskeline.IO
import Text.Parsec (runParser)
import Tn.Builtins
import Tn.Debug
import Tn.Eval
import Tn.Scalar
import Tn.Scope
import Tn.Script

data Opts = Opts
  { scriptFiles :: [String],
    precision :: Maybe Int,
    sciNotation :: Bool,
    delim :: Maybe String,
    noUnits :: Bool,
    exprStrings :: [String]
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
        exprStrings = def &= args &= typ "EXPRESSION [ARGS...]"
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
        "  tn '2 * [sin 45 deg]'",
        "  tn '100 hz * _ m : mph' < values.txt"
      ]
    &= noAtExpand

motd :: String
motd = printf "Tournesol v%d.%d.%d, (c) Jeffrey Massung" major minor patch
  where
    major = 0 :: Int
    minor = 1 :: Int
    patch = 0 :: Int

printFormat :: Opts -> Scalar -> String
printFormat opts (Scalar x _) = "%0." ++ prec ++ (if sciNotation opts then "g" else "f")
  where
    prec =
      if denominator x == 1
        then "0"
        else show (fromMaybe 2 $ precision opts)

printAns :: Opts -> Scalar -> IO Scalar
printAns opts ans@(Scalar _ u) = do
  if isJust u && not opts.noUnits
    then printf (printFormat opts ans ++ " %U\n") ans ans
    else printf (printFormat opts ans ++ "\n") ans
  return ans

runExpr :: Opts -> Scope -> Scalar -> String -> IO Scalar
runExpr opts scope ans s =
  case evalWithScope s ans scope of
    Left err -> putStrLn err >> return ans
    Right ans' -> printAns opts ans'

repl :: Opts -> Scope -> Scalar -> InputState -> IO ()
repl opts scope ans is = do
  queryInput is (getInputLine ">> ") >>= \case
    Nothing -> repl opts scope ans is
    Just s -> do
      ans' <- runExpr opts scope ans s
      repl opts scope ans' is

runInteractive :: Opts -> Scope -> IO InputState -> IO ()
runInteractive opts scope is = do
  putStrLn motd
  bracketOnError is cancelInput $ repl opts scope 0

loadScripts :: [String] -> IO Scope
loadScripts = foldM load mempty
  where
    load :: Scope -> String -> IO Scope
    load scope file =
      loadScriptFile file scope >>= \case
        Left err -> error err
        Right scope' -> return scope'

main :: IO ()
main = do
  opts <- getOpts

  -- load all the script files
  scope <- case opts.scriptFiles of
    [] -> return defaultScope
    files -> loadScripts files

  -- run supplied expressions or enter read-eval-print-loop
  case opts.exprStrings of
    [] -> runInteractive opts scope $ initializeInput defaultSettings
    exprs -> mapM_ (runExpr opts scope 0) exprs
