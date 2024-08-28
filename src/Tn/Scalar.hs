{-# LANGUAGE MultiWayIf #-}

module Tn.Scalar where

import qualified Data.Map.Strict as M
import Text.Printf
import Tn.Conv
import Tn.Error
import Tn.Units
import Prelude hiding ((~~))

data Scalar
  = Scalar Rational (Maybe Units)
  | InvalidScalar NumericalError

instance Eq Scalar where
  (==) (Scalar x Nothing) (Scalar y _) = x == y
  (==) (Scalar x _) (Scalar y Nothing) = x == y
  (==) (Scalar x (Just ux)) (Scalar y (Just uy)) = x == y && dims ux == dims uy
  (==) _ _ = False

instance Show Scalar where
  show x@(Scalar _ Nothing) = printf "%g" x
  show x@(Scalar _ _) = printf "%g %U" x x
  show (InvalidScalar err) = show err

instance PrintfArg Scalar where
  formatArg (InvalidScalar err) fmt
    | fmtChar (vFmt 'd' fmt) == 'd' = formatString (show err) fmt {fmtChar = 's'}
    | fmtChar (vFmt 'e' fmt) == 'e' = formatString (show err) fmt {fmtChar = 's'}
    | fmtChar (vFmt 'f' fmt) == 'f' = formatString (show err) fmt {fmtChar = 's'}
    | fmtChar (vFmt 'g' fmt) == 'g' = formatString (show err) fmt {fmtChar = 's'}
    | fmtChar (vFmt 'U' fmt) == 'U' = formatString "" fmt {fmtChar = 's'}
    | otherwise = errorBadFormat $ fmtChar fmt
  formatArg (Scalar x u) fmt
    | fmtChar (vFmt 'd' fmt) == 'd' = formatInteger (floor x :: Integer) fmt
    -- print the scalar as a float
    | fmtChar (vFmt 'e' fmt) == 'e' = formatRealFloat (fromRational x :: Double) fmt
    | fmtChar (vFmt 'f' fmt) == 'f' = formatRealFloat (fromRational x :: Double) fmt
    | fmtChar (vFmt 'g' fmt) == 'g' = formatRealFloat (fromRational x :: Double) fmt
    -- print the units of the scalar
    | fmtChar (vFmt 'U' fmt) == 'U' = formatString (maybe "" show u) fmt {fmtChar = 's'}
    | otherwise = errorBadFormat $ fmtChar fmt

instance Ord Scalar where
  compare (InvalidScalar _) (InvalidScalar _) = EQ
  compare _ (InvalidScalar _) = LT
  compare (InvalidScalar _) _ = GT
  compare (Scalar x Nothing) (Scalar y _) = compare x y
  compare (Scalar x _) (Scalar y Nothing) = compare x y
  compare (Scalar x (Just ux)) (Scalar y (Just uy)) =
    if ux ~= uy
      -- when dimensions match, convert to base units and compare
      then
        let x' = convToBase x $ unitsConv ux
            y' = convToBase y $ unitsConv uy
         in compare x' y'
      -- when the dimensions don't match, just compare magnitudes
      else compare x y

instance Num Scalar where
  fromInteger n = Scalar (fromInteger n) mempty

  -- add scalars
  (+) x@(InvalidScalar _) _ = x
  (+) _ y@(InvalidScalar _) = y
  (+) (Scalar x Nothing) (Scalar y uy) = Scalar (x + y) uy
  (+) (Scalar x ux) (Scalar y Nothing) = Scalar (x + y) ux
  (+) (Scalar x (Just ux)) (Scalar y (Just uy)) =
    if
      | ux == uy -> Scalar (x + y) (Just ux)
      | ux ~= uy ->
          let cx = unitsConv ux
              cy = unitsConv uy
              y' = convFromBase (convToBase y cy) cx
           in Scalar (x + y') $ Just ux
      | otherwise -> InvalidScalar DisparateUnits

  -- multiply scalars
  (*) x@(InvalidScalar _) _ = x
  (*) _ y@(InvalidScalar _) = y
  (*) (Scalar x ux) (Scalar y Nothing) = Scalar (x * y) ux
  (*) (Scalar x Nothing) (Scalar y uy) = Scalar (x * y) uy
  (*) (Scalar x (Just ux)) (Scalar y (Just uy)) =
    if ux == uy
      then Scalar (x * y) (Just $ ux <> uy)
      else
        let x' = convToBase x $ unitsConv ux
            y' = convToBase y $ unitsConv uy
            u' = mergeUnits uy ux
            c' = unitsConv u'
         in Scalar (convFromBase (x' * y') c') $ Just u'

  -- mapped functions
  negate = mapScalar negate
  abs = mapScalar abs
  signum = mapScalar signum . dropUnits

quietNaN :: Rational
quietNaN = unsafeCoerce (0x7ff8000000000000 :: Word64)

instance Real Scalar where
  toRational (InvalidScalar _) = quietNaN
  toRational (Scalar x _) = x

instance Fractional Scalar where
  fromRational r = Scalar (fromRational r) Nothing

  -- division, need to override for div by 0
  (/) x@(InvalidScalar _) _ = x
  (/) _ y@(InvalidScalar _) = y
  (/) x@(Scalar _ _) y@(Scalar _ _) = x * recip y

  -- scalar inverse
  recip (Scalar 0 _) = InvalidScalar DivByZero
  recip (Scalar x u) = Scalar (recip x) (fmap recipUnits u)
  recip x = x

mapFloating :: (Double -> Double) -> (Scalar -> Scalar)
mapFloating f = mapScalar $ toRational . (f . fromRational :: Rational -> Double)

dropUnits :: Scalar -> Scalar
dropUnits x@(InvalidScalar _) = x
dropUnits (Scalar n _) = Scalar n Nothing

mapScalar :: (Rational -> Rational) -> Scalar -> Scalar
mapScalar _ x@(InvalidScalar _) = x
mapScalar f (Scalar x u) = Scalar (f x) u

convertTo :: Units -> Scalar -> Scalar
convertTo _ x@(InvalidScalar _) = x
convertTo to (Scalar n Nothing) = Scalar n $ Just to
convertTo to (Scalar n (Just from)) =
  if from ~= to
    then
      let n' = convFromBase (convToBase n $ unitsConv from) (unitsConv to)
       in Scalar n' $ Just to
    else InvalidScalar DisparateUnits

powScalar :: Scalar -> Scalar -> Scalar
powScalar x@(InvalidScalar _) _ = x
powScalar _ (InvalidScalar _) = InvalidScalar IllegalExponent
powScalar _ (Scalar _ (Just _)) = InvalidScalar IllegalExponent
powScalar (Scalar x u) (Scalar n _)
  | n == 0 = 1
  | denominator n == 1 = Scalar (x ^^ numerator n) u'
  | otherwise =
      let x' = fromRational x :: Double
          n' = fromRational n :: Double
       in Scalar (toRational $ x' ** n') u'
  where
    u' = case u of
      Nothing -> Nothing
      Just (Units m) -> Just $ Units $ M.map (* n) m
