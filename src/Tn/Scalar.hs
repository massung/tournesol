{-# LANGUAGE MultiWayIf #-}

module Tn.Scalar where

import qualified Data.Map.Strict as M
import GHC.Real (infinity)
import Text.Printf
import Tn.Conv
import Tn.Error
import Tn.Units
import Prelude hiding ((~~))

data Scalar = Scalar Rational (Maybe Units)

instance Eq Scalar where
  (==) (Scalar x Nothing) (Scalar y _) = x == y
  (==) (Scalar x _) (Scalar y Nothing) = x == y
  (==) (Scalar x (Just ux)) (Scalar y (Just uy)) = x == y && dims ux == dims uy

instance Show Scalar where
  show x@(Scalar _ Nothing) = printf "%g" x
  show x@(Scalar _ _) = printf "%g %U" x x

instance PrintfArg Scalar where
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
  compare (Scalar x Nothing) (Scalar y _) = compare x y
  compare (Scalar x _) (Scalar y Nothing) = compare x y
  compare (Scalar x (Just ux)) (Scalar y (Just uy)) =
    if ux ~= uy
      then
        let x' = convToBase x $ unitsConv ux
            y' = convToBase y $ unitsConv uy
         in compare x' y'
      else error "disparate units"

instance Num Scalar where
  fromInteger n = Scalar (fromInteger n) mempty

  -- add scalars
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
      | otherwise -> error "disparate units"

  -- multiply scalars
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
  negate (Scalar x u) = Scalar (negate x) u
  abs (Scalar x u) = Scalar (abs x) u
  signum (Scalar x _) = Scalar (signum x) Nothing

instance Real Scalar where
  toRational (Scalar x _) = x

instance Fractional Scalar where
  fromRational r = Scalar (fromRational r) Nothing

  -- division, need to override for div by 0
  (/) (Scalar _ u) (Scalar 0 _) = Scalar infinity u
  (/) x y = x * recip y

  -- scalar inverse
  recip (Scalar 0 u) = Scalar infinity u
  recip (Scalar x u) = Scalar (recip x) (fmap recipUnits u)
    where
      recipUnits (Units m) = Units $ M.map negate m

convertTo :: Scalar -> Units -> Either Error Scalar
convertTo (Scalar n Nothing) to = Right (Scalar n $ Just to)
convertTo (Scalar n (Just from)) to =
  if from ~= to
    then
      let n' = convFromBase (convToBase n $ unitsConv from) (unitsConv to)
       in Right (Scalar n' $ Just to)
    else Left $ WrongDims (dims from) (dims to)

powScalar :: Scalar -> Scalar -> Either Error Scalar
powScalar _ (Scalar _ (Just _)) = Left IllegalExponent
powScalar (Scalar x u) (Scalar n _)
  | n == 0 = Right 1
  | denominator n == 1 = Right $ Scalar (x ^^ numerator n) u'
  | otherwise =
      let x' = fromRational x :: Double
          n' = fromRational n :: Double
       in Right $ Scalar (toRational $ x' ** n') u'
  where
    u' = case u of
      Nothing -> Nothing
      Just (Units m) -> Just $ Units $ M.map (* n) m
