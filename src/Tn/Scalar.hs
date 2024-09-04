module Tn.Scalar where

import GHC.Real (infinity)
import Text.Printf
import Tn.Dims
import Tn.Unit

data Scalar = Scalar Rational (Maybe Units)

instance Eq Scalar where
  (==) (Scalar x Nothing) (Scalar y _) = x == y
  (==) (Scalar x _) (Scalar y Nothing) = x == y
  (==) (Scalar x ux) (Scalar y uy) = x == y && ux == uy

instance Ord Scalar where
  compare (Scalar a ux) (Scalar b uy) = assert (ux == uy) $ compare a b

instance Show Scalar where
  show x@(Scalar _ Nothing) = printf "%g" x
  show x@(Scalar _ _) = printf "%g %U" x x

instance PrintfArg Scalar where
  formatArg (Scalar x u) fmt
    | fmtChar (vFmt 'd' fmt) == 'd' = formatInteger (floor x) fmt
    -- print the scalar as a float
    | fmtChar (vFmt 'e' fmt) == 'e' = formatRealFloat (fromRational x :: Double) fmt
    | fmtChar (vFmt 'f' fmt) == 'f' = formatRealFloat (fromRational x :: Double) fmt
    | fmtChar (vFmt 'g' fmt) == 'g' = formatRealFloat (fromRational x :: Double) fmt
    -- print the units of the scalar
    | fmtChar (vFmt 'U' fmt) == 'U' = formatString (maybe "" show u) fmt {fmtChar = 's'}
    | otherwise = errorBadFormat $ fmtChar fmt

instance Num Scalar where
  fromInteger i = Scalar (fromInteger i) Nothing

  -- addition requires units to be identical
  (+) (Scalar x ux) (Scalar y uy)
    | isNothing ux = Scalar (x + y) uy
    | isNothing uy = Scalar (x + y) ux
    | otherwise = assert (ux == uy) (Scalar (x + y) ux)

  -- multiplication requires units of the same dimensions be identical
  (*) (Scalar x ux) (Scalar y uy)
    | isNothing ux = Scalar (x * y) uy
    | isNothing uy = Scalar (x * y) ux
    | otherwise = assert (maybe True verifyUnits u) (Scalar (x * y) u)
    where
      u = ux <> uy

  -- numeric mapping
  negate = mapScalar negate
  abs = mapScalar abs
  signum = dropUnits . mapScalar signum

instance Real Scalar where
  toRational (Scalar n _) = n

instance Fractional Scalar where
  fromRational r = Scalar (fromRational r) Nothing

  -- reciprocal of scalar and units
  recip (Scalar 0 _) = invalidScalar
  recip (Scalar n u) = Scalar (recip n) (fmap recipDims u)

invalidScalar :: Scalar
invalidScalar = Scalar infinity Nothing

scalarUnits :: Scalar -> Maybe Units
scalarUnits (Scalar _ u) = u

dropUnits :: Scalar -> Scalar
dropUnits (Scalar n _) = Scalar n Nothing

mapScalar :: (Rational -> Rational) -> Scalar -> Scalar
mapScalar f (Scalar n u) = Scalar (f n) u

powScalar :: Scalar -> Int -> Scalar
powScalar (Scalar x u) n = Scalar (x ^^ n) $ fmap (*^ n) u
