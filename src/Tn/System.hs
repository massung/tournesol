{-# LANGUAGE OverloadedStrings #-}

module Tn.System where

data System
  = Imperial
  | Metric
  | Binary
  deriving (Eq, Show)

siPrefixes :: [(String, String, Rational)]
siPrefixes =
  [ ("atto", "a", 1 % 1000000000000000000),
    ("femto", "f", 1 % 1000000000000000),
    ("pico", "p", 1 % 1000000000000),
    ("nano", "n", 1 % 1000000000),
    ("micro", "u", 1 % 1000000),
    ("milli", "m", 1 % 1000),
    ("centi", "c", 1 % 100),
    ("deci", "d", 1 % 10),
    ("deca", "da", 10),
    ("hecto", "h", 100),
    ("kilo", "k", 1000),
    ("mega", "M", 1000000),
    ("giga", "G", 1000000000),
    ("tera", "T", 1000000000000),
    ("peta", "P", 1000000000000000),
    ("exa", "E", 1000000000000000000)
  ]

binaryPrefixes :: [(String, String, Rational)]
binaryPrefixes =
  [ ("kilo", "k", 1024),
    ("mega", "M", 1048576),
    ("giga", "G", 1073741824),
    ("tera", "T", 1099511627776),
    ("peta", "P", 1125899906842624),
    ("exa", "E", 1152921504606846976)
  ]
