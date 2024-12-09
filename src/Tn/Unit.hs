{-# LANGUAGE OverloadedLists #-}

module Tn.Unit where

import qualified Data.Map as M
import Data.Tuple.Extra
import Tn.Dims
import Tn.Symbol
import Prelude hiding (singleton)

data Unit = Unit Symbol Base

data Base
  = Base Symbol
  | Derived Rational Units
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
unitWithPrefix :: Unit -> String -> Rational -> Unit
unitWithPrefix (Unit sym base) prefix r =
  Unit (intern $ prefix <> unintern sym) $ case base of
    Derived r' units -> Derived (r' * r) units
    _ -> base

-- returns the fundamental dimensions of units; eg, lbf -> mass length / time^2
baseDims :: Units -> Dims Symbol
baseDims = foldDims reduce mempty
  where
    reduce :: Dims Symbol -> Unit -> Int -> Dims Symbol
    reduce dims (Unit _ (Base sym)) e = dims <> [(sym, e)]
    reduce dims (Unit _ (Derived _ u')) e = dims <> baseDims u' *^ e

-- returns the base units and linear conversion scale
baseUnits :: Units -> (Rational, Units)
baseUnits = foldDims reduce (1, mempty)
  where
    reduce :: (Rational, Units) -> Unit -> Int -> (Rational, Units)
    reduce (r, units) u@(Unit _ (Base _)) e = (r, units <> [(u, e)])
    reduce (r, units) (Unit _ (Derived f u')) e =
      let (r', units') = baseUnits (u' *^ e)
       in (r * (f ^^ e) * r', units <> units')

-- maps dimensions to a base unit and exponent
mapBaseUnitDims :: Units -> Map Symbol (Unit, Int)
mapBaseUnitDims = foldDims reduce mempty
  where
    reduce :: Map Symbol (Unit, Int) -> Unit -> Int -> Map Symbol (Unit, Int)
    reduce m u@(Unit _ (Base dim)) e = M.insertWith tally dim (u, e) m
    reduce m u@(Unit _ (Derived _ u')) e =
      case baseDims u' of
        [(dim, _)] -> M.insertWith tally dim (u, e) m
        _ -> M.unionWith tally m $ mapBaseUnitDims (u' *^ e)

    tally :: (Unit, Int) -> (Unit, Int) -> (Unit, Int)
    tally (a, an) (b, bn) = assert (a == b) (a, an + bn)

-- verifies that each fundamental dimension only occurs once
verifyUnits :: Units -> Bool
verifyUnits = {-verify-} uncurry (&&) . both verify . partitionDims
  where
    verify :: Units -> Bool
    verify = all (== 1) . foldDims reduce mempty . snd . baseUnits

    reduce :: Map Symbol Int -> Unit -> Int -> Map Symbol Int
    reduce m u _ = foldDims count m . baseDims . singleton $ u

    count :: Map Symbol Int -> Symbol -> Int -> Map Symbol Int
    count dims s _ = M.alter (Just . maybe 1 (+ 1)) s dims

-- given two units, return base units that need converted (from, to, exponent)
unitsToConv :: Units -> Units -> [(Unit, Unit, Int)]
unitsToConv from to =
  let from' = mapBaseUnitDims from
      to' = mapBaseUnitDims to
      m = M.intersectionWith (,) from' to'
   in [(a, b, e) | ((a, e), (b, _)) <- M.elems m, a /= b]

-- supply units to convert mapping to change from units to converted
convUnits :: [(Unit, Unit, Int)] -> Units -> Units
convUnits unitsMap (Dims m) = Dims $ foldl' replaceUnit m unitsMap
  where
    replaceUnit m' (u, u', e) = M.insert u' e $ M.delete u m'
