{-# LANGUAGE OverloadedLists #-}

module Tn.Unit where

import qualified Data.Map as M
import Tn.Dims
import Tn.Symbol
import Prelude hiding (singleton)

data Unit = Unit Symbol Base

data Base
  = Base Symbol
  | Derived Units
  deriving (Eq, Show)

type Units = Dims Unit

instance Eq Unit where
  (==) (Unit a _) (Unit b _) = a == b

instance Ord Unit where
  compare (Unit a _) (Unit b _) = compare a b

instance Hashable Unit where
  hashWithSalt salt (Unit u _) = hashWithSalt salt u
  hash (Unit u _) = hash u

instance Show Unit where
  show (Unit u _) = show u

-- create a new unit with the same base
unitWithPrefix :: Unit -> String -> Unit
unitWithPrefix (Unit sym base) p = Unit (intern $ p <> unintern sym) base

-- returns the fundamental dimensions of units; eg, N -> mass length / time^2
baseDims :: Units -> Dims Symbol
baseDims = foldDims reduce mempty
  where
    reduce :: Dims Symbol -> Unit -> Int -> Dims Symbol
    reduce dims (Unit _ (Base sym)) n = dims <> [(sym, n)]
    reduce dims (Unit _ (Derived units')) n = dims <> (baseDims units' *^ n)

-- returns the most fundamental units; eg, N^2 -> kg^2 m^2 / s^4
baseUnits :: Units -> Units
baseUnits = foldDims reduce mempty
  where
    reduce :: Units -> Unit -> Int -> Units
    reduce units u@(Unit _ (Base _)) n = units <> [(u, n)]
    reduce units (Unit _ (Derived units')) n = units <> (baseUnits units' *^ n)

-- returns a map of dimension to base units
baseUnitDims :: Units -> Map Symbol (Unit, Int)
baseUnitDims = foldDims reduce mempty
  where
    reduce :: Map Symbol (Unit, Int) -> Unit -> Int -> Map Symbol (Unit, Int)
    reduce m u@(Unit _ (Base dim)) e = m <> [(dim, (u, e))]
    reduce m (Unit _ (Derived units')) e = m <> baseUnitDims (units' *^ e)

-- true if *all* dimensions are the same - units can be perfectly converted
(~=) :: Units -> Units -> Bool
(~=) a b = baseDims a == baseDims b

-- true if *any* dimensions are disparate
(~/=) :: Units -> Units -> Bool
(~/=) a b = not $ a ~= b

-- verifies that each fundamental dimension only occurs once
verifyUnits :: Units -> Bool
verifyUnits = all (== 1) . foldDims reduce mempty . baseUnits
  where
    reduce :: Map Symbol Int -> Unit -> Int -> Map Symbol Int
    reduce m u _ = foldDims count m . baseDims . singleton $ u

    count :: Map Symbol Int -> Symbol -> Int -> Map Symbol Int
    count dims s _ = M.alter (Just . maybe 1 (+ 1)) s dims

-- given two units, return base units that need converted (from, to)
unitsToConv :: Units -> Units -> [(Unit, Unit, Int)]
unitsToConv from to =
  let from' = baseUnitDims from
      to' = baseUnitDims to
      m = M.intersectionWith (,) from' to'
   in [(a, b, e) | ((a, e), (b, _)) <- M.elems m, a /= b]

-- supply units to convert mapping to change from units to converted
convUnits :: [(Unit, Unit, Int)] -> Units -> Units
convUnits unitsMap (Dims m) = Dims $ foldl' replaceUnit m unitsMap
  where
    replaceUnit m' (u, u', e) = M.insert u' e $ M.delete u m'
