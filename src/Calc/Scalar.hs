module Calc.Scalar where

import Calc.Conv
import Calc.Dims
import Calc.Error
import Calc.Units
import Data.Ratio
import Text.Printf

data Scalar = Scalar !Rational !Dims !Units
  deriving (Eq)

instance Show Scalar where
  show (Scalar x d u)
    | nullUnits u = show (fromRational x)
    | otherwise = show (fromRational x) ++ " " ++ show u

instance PrintfArg Scalar where
  formatArg (Scalar x d u) fmt
    | fmtChar (vFmt 'g' fmt) `elem` ['e' .. 'g'] = formatRealFloat (fromRational x) fmt
    | fmtChar (vFmt 'U' fmt) == 'U' = formatString (show u) fmt {fmtChar = 's'}
    | otherwise = errorBadFormat $ fmtChar fmt

instance Semigroup Scalar where
  (<>) a b = a * b

instance Ord Scalar where
  compare (Scalar x dx ux) (Scalar y dy uy) =
    if nullDims dx || nullDims dy || ux == uy
      then compare x y
      else error "Cannot compare disparate units"

instance Num Scalar where
  fromInteger n = Scalar (fromInteger n) mempty mempty

  -- add scalars
  (+) (Scalar x dx ux) (Scalar y dy uy)
    | nullDims dy = Scalar (x + y) dx ux
    | nullDims dx = Scalar (x + y) dy uy
    | ux == uy = Scalar (x + y) dx ux
    | otherwise = error "Cannot add disparate units"

  -- multiply scalars
  (*) (Scalar x dx ux) (Scalar y dy uy) = Scalar (x * y) (dx <> dy) (ux <> uy)

  -- mapped functions
  negate = mapScalar negate
  abs = mapScalar abs
  signum = mapScalar signum

instance Real Scalar where
  toRational (Scalar x _ _) = x

instance Fractional Scalar where
  fromRational r = Scalar (fromRational r) mempty mempty

  -- scalar inverse
  recip (Scalar x d u) = Scalar (recip x) (recipDims d) (recipUnits u)

scalar x units = Scalar (toRational x) (dims units) units

fromReal f = Scalar (toRational f) mempty mempty

fromBool True = Scalar 1 mempty mempty
fromBool False = Scalar 0 mempty mempty

fromUnits u = Scalar 1 (dims u) u

mapScalar f (Scalar x d u) = Scalar (f x) d u

powScalar (Scalar x _ u) (Scalar n d _)
  | not $ nullDims d = Left IllegalExponent
  | n == 0 = Right 1
  | denominator n == 1 = Right $ Scalar (x ^^ numerator n) (dims u') u'
  | otherwise = Right $ Scalar (toRational $ fromRational x ** fromRational n) (dims u') u'
  where
    u' = mapUnits (* n) u

harmonize s@(Scalar x d from) to
  | nullUnits from || nullUnits to = Right s
  | nullDims d = Right $ Scalar x (dims to) to
  | otherwise = convert s $ harmonizeUnits from to

convert (Scalar x d from) to
  | nullUnits to = Right $ Scalar x d from
  | nullDims d = Right $ Scalar x (dims to) to
  | d == dims to = Right $ Scalar (applyConv (unitsConvScale from to) x) d to
  | otherwise = Left $ ConversionError from to
