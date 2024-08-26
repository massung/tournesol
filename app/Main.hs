{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import System.Console.CmdArgs
import System.Console.Haskeline
import System.Console.Haskeline.IO
import Text.Parsec (runParser)
import Tn.Eval
import Tn.Expr
import Tn.Parser
import Tn.Scalar
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
      { scriptFiles = def &= explicit &= name "f" &= name "functions" &= typ "FILE" &= help "Load Tournesol script functions file",
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
        "  tn '500 N * 10 ft to BTU'",
        "  tn '10 GB / 2 hr to MB/s'",
        "  tn '2 * (1500 cm)^2 to acre'",
        "  tn '30 W * 6 min to J'",
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
printFormat _ (InvalidScalar e) = show e
printFormat opts (Scalar x _) = "%0." ++ prec ++ (if sciNotation opts then "g" else "f")
  where
    prec =
      if denominator x == 1
        then "0"
        else show (fromMaybe 2 $ precision opts)

printAns :: Opts -> Scalar -> IO ()
printAns _ (InvalidScalar e) = print e
printAns opts x@(Scalar _ u) =
  if isJust u
    then printf (printFormat opts x ++ " %U\n") x x
    else printf (printFormat opts x ++ "\n") x

prompt :: Opts -> InputState -> Script -> IO Expr
prompt opts hd script = do
  queryInput hd (getInputLine ">> ") >>= \case
    Nothing -> prompt opts hd script
    Just s ->
      let expr = runParser exprParser script "" s
       in either (\e -> print e >> prompt opts hd script) return expr

repl :: Opts -> Script -> [Scalar] -> InputState -> IO ()
repl opts script xs hd = do
  expr <- prompt opts hd script

  -- catch exceptions during evaluation
  case eval xs expr of
    Left err -> print err >> repl opts script xs hd
    Right ans -> printAns opts ans >> repl opts script [ans] hd

main :: IO ()
main = do
  opts <- getOpts

  -- TODO: load all the scripts
  let inputState = initializeInput defaultSettings
      script = defaultScript

  -- show the motd
  putStrLn motd

  -- run the program or repl
  bracketOnError inputState cancelInput $ repl opts script []
