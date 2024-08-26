{-# LANGUAGE OverloadedStrings #-}

module Tn.Conv where

{-
This module handles unit conversions.

Operations performed on units is always done at using the base units.
Units are either the Base units for a Dimension or a pair of functions
used for converting toBase or fromBase.

For example:

meters = Base
feet = Linear (1250 % 381)  <- (units : base) ratio!

celcius = Base
fahrenheit = Conv $ ConvFunction fToC cToF
-}

-- self-documenting type
type UnitsPerBase = Rational

-- self-documenting type
type ToBaseConv = Rational -> Rational

-- self-documenting type
type FromBaseConv = Rational -> Rational

data Conv
  = Base
  | Linear UnitsPerBase
  | Conv FromBaseConv ToBaseConv

instance Semigroup Conv where
  (<>) Base y = y
  (<>) x Base = x
  (<>) (Linear x) (Linear y) = Linear $ x * y
  (<>) (Linear x) (Conv from to) = Conv (from . (* x)) (to . (/ x))
  (<>) (Conv from to) (Linear y) = Conv ((* y) . from) ((/ y) . to)
  (<>) (Conv fFrom fTo) (Conv gFrom gTo) = Conv (gFrom . fFrom) (gTo . fTo)

instance Monoid Conv where
  mempty = Base

siConvs :: [(String, String, Conv)]
siConvs =
  [ ("atto", "a", Linear 1e18),
    ("femto", "f", Linear 1e15),
    ("pico", "p", Linear 1e12),
    ("nano", "n", Linear 1e9),
    ("micro", "u", Linear 1e6),
    ("milli", "m", Linear 1e3),
    ("centi", "c", Linear 1e2),
    ("deci", "d", Linear 1e1),
    ("deca", "da", Linear 1e-1),
    ("hecto", "h", Linear 1e-2),
    ("kilo", "k", Linear 1e-3),
    ("mega", "M", Linear 1e-6),
    ("giga", "G", Linear 1e-9),
    ("tera", "T", Linear 1e-12),
    ("peta", "P", Linear 1e-15),
    ("exa", "E", Linear 1e-18)
  ]

storageConvs :: [(String, String, Conv)]
storageConvs =
  [ ("kilo", "k", Linear (1 % 1024)),
    ("mega", "M", Linear (1 % 1048576)),
    ("giga", "G", Linear (1 % 1073741824)),
    ("tera", "T", Linear (1 % 1099511627776)),
    ("peta", "P", Linear (1 % 1125899906842624)),
    ("exa", "E", Linear (1 % 1152921504606846976))
  ]

-- raises a conversion by a power
powConv :: Rational -> Conv -> Conv
powConv 0 _ = Conv (const 1) (const 1)
powConv _ Base = Base
powConv 1 x = x
powConv n (Linear x) =
  if denominator n == 1
    then Linear $ x ^^ numerator n
    else Linear $ toRational (fromRational x ** fromRational n :: Double)
powConv _ _ = error "cannot exponentiate non-linear conversion"

-- apply a conversion to a rational to return the base units value
convToBase :: Rational -> Conv -> Rational
convToBase n Base = n
convToBase n (Linear r) = n / r
convToBase n (Conv from _) = from n

-- apply a conversion to a base rational to return the units value
convFromBase :: Rational -> Conv -> Rational
convFromBase n Base = n
convFromBase n (Linear r) = n * r
convFromBase n (Conv _ to) = to n
