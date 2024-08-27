{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Tn.Units where

import qualified Data.Map.Strict as M
import Data.Symbol
import Tn.Conv
import Tn.Dims

data Unit = Unit
  { _symbol :: Symbol,
    _name :: String,
    _dim :: Dim,
    _conv :: Conv
  }

instance Eq Unit where
  (==) a b = a._symbol == b._symbol

instance Ord Unit where
  compare a b = compare a._symbol b._symbol

instance Show Unit where
  show = unintern . _symbol

-- unit : exponent
type UnitMap = M.Map Unit Rational

newtype Units = Units UnitMap
  deriving (Eq)

instance Semigroup Units where
  (<>) (Units a) (Units b) = Units $ M.filter (/= 0) $ M.unionWith (+) a b

instance Monoid Units where
  mempty = Units mempty

instance Show Units where
  show (Units m) =
    let (num, den) = M.partition (> 0) m
     in if
          | null num -> show' den
          | null den -> show' num
          | otherwise -> show' num ++ "/" ++ show' (M.map abs den)
    where
      show' units = unwords [showExp u | u <- M.toList units]

      -- show a single unit with optional exponent
      showExp (u, 1) = show u
      showExp (u, n) =
        if denominator n == 1
          then show u ++ "^" ++ show (numerator n)
          else show u ++ "^" ++ show (fromRational n :: Double)

-- reciprocol of <>
(</>) :: Units -> Units -> Units
(</>) a (Units m) = a <> Units (M.map negate m)

-- returns the fundamental dimensions of the units
dims :: Units -> Dims
dims (Units m) = M.foldlWithKey' reduceUnits mempty m
  where
    reduceUnits :: Dims -> Unit -> Rational -> Dims
    reduceUnits d u n =
      let (Dims d') = fundamentalDims u._dim
       in d <> Dims (M.map (* n) d')

-- verifies that each fundamental dimension only occurs once
verifyUnits :: Units -> Bool
verifyUnits (Units m) = all (== 1) $ M.foldlWithKey' countDims mempty m
  where
    countDims :: Map Dim Int -> Unit -> Rational -> Map Dim Int
    countDims d u _ =
      let (Dims f) = fundamentalDims u._dim
       in foldl' (flip (M.alter tally)) d $ M.keys f

    tally :: Maybe Int -> Maybe Int
    tally x = fmap (+ 1) x <|> Just 1

-- true if *all* dimensions are the same (ie, addition OK)
(~=) :: Units -> Units -> Bool
(~=) a b = dims a == dims b

-- true if all *shared* dimensions are the same
(~~) :: Units -> Units -> Bool
(~~) a b =
  let (Dims da) = dims a
      (Dims db) = dims b
   in and $ M.intersectionWith (==) da db

-- create derived (eg, SI) units from a base unit
derivedUnits :: [(String, String, Conv)] -> Unit -> [Unit]
derivedUnits convs u = u : [derivedUnit n p c | (n, p, c) <- convs]
  where
    base :: String
    base = unintern u._symbol

    derivedUnit :: String -> String -> Conv -> Unit
    derivedUnit n p c = u {_symbol = intern (p ++ base), _name = n ++ u._name, _conv = c}

-- create metric units derived from a base unit
siUnits :: Unit -> [Unit]
siUnits = derivedUnits siConvs

-- create metric units that are < 1 from a base unit
subSIUnits :: Unit -> [Unit]
subSIUnits = derivedUnits [c | c@(_, _, Linear n) <- siConvs, n >= 1]

-- create storage units derived from a base unit
storageUnits :: Unit -> [Unit]
storageUnits = derivedUnits storageConvs

-- returns the conversion of units across all dimensions
unitsConv :: Units -> Conv
unitsConv (Units m) = M.foldlWithKey' reduceConv mempty m
  where
    reduceConv :: Conv -> Unit -> Rational -> Conv
    reduceConv conv u e = conv <> powConv e u._conv

-- like (<>), but favors the units of _to when sharing dimensions
mergeUnits :: Units -> Units -> Units
mergeUnits from@(Units a) to@(Units b) =
  let (Dims d) = dims $ from <> to
   in Units $ M.mapKeys (dToU $ M.keys b ++ M.keys a) d
  where
    dToU :: [Unit] -> Dim -> Unit
    dToU units dim = fromJust $ find ((== dim) . _dim) units
