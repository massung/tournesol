{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Tn.Builtins where

import Data.FileEmbed
import Text.Parsec
import Text.Printf
import Tn.Scope
import Tn.Script

defaultScript :: String
defaultScript = $(embedStringFile "scripts/default.tn")

imperialScript :: String
imperialScript = $(embedStringFile "scripts/imperial.tn")

astronomicalScript :: String
astronomicalScript = $(embedStringFile "scripts/astronomical.tn")

maritimeScript :: String
maritimeScript = $(embedStringFile "scripts/maritime.tn")

storageScript :: String
storageScript = $(embedStringFile "scripts/storage.tn")

defaultScope :: Scope
defaultScope =
  case foldr load (Right mempty) scripts of
    Left err -> error err
    Right scope -> scope
  where
    load :: (String, String) -> Either String Scope -> Either String Scope
    load (fn, s) = either Left (loadScript fn s)

    scripts :: [(String, String)]
    scripts =
      [ ("storage.tn", storageScript),
        ("astronomical.tn", astronomicalScript),
        ("maritime.tn", maritimeScript),
        ("imperial.tn", imperialScript),
        ("default.tn", defaultScript)
      ]

-- _eV :: Unit
-- _eV = Unit "eV" _energy

-- _Tf :: Unit
-- _Tf = Unit "Tf" _temperature
-- where
--   cToF x = x * (9 % 5) + 32
--   fToC x = (x - 32) * (5 % 9)

-- _Tk :: Unit
-- _Tk = Unit "Tk" _temperature
-- where
--   cToK x = x + 273.15
--   kToC x = x - 273.15

-- _Tr :: Unit
-- _Tr = Unit "Tr" _temperature
-- where
--   cToR x = x * (9 % 5) + 491.67
--   rToC x = (x - 491.67) * (5 % 9)
