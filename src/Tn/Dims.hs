{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedLists #-}

module Tn.Dims where

{-
Dimensions are the most fundamental type for units. Examples include length,
mass, and time. They are also associative and can be combined:

length = Fundamental "length"
mass = Fundamental "mass"
duration = Fundamental "duration"

area = Derived "area" [(length, 2)]
force = Derived "force" [(mass, 1), (length, 1), (duration, -2)]
-}

import qualified Data.Map.Strict as M
import Data.Symbol

data Dim
  = Fundamental Symbol
  | Derived Symbol Dims

instance Show Dim where
  show (Fundamental a) = unintern a
  show (Derived a _) = unintern a

instance Eq Dim where
  (==) (Fundamental a) (Fundamental b) = a == b
  (==) (Derived a x) (Derived b y) = a == b || x == y
  (==) _ _ = False

instance Ord Dim where
  compare (Fundamental a) (Fundamental b) = compare a b
  compare (Derived a _) (Derived b _) = compare a b
  compare (Fundamental a) (Derived b _) = compare a b
  compare (Derived a _) (Fundamental b) = compare a b

-- dimension : exponent
type DimMap = M.Map Dim Rational

newtype Dims = Dims DimMap
  deriving (Eq)

instance Show Dims where
  show (Dims d) = showDimsMap d

instance Semigroup Dims where
  (<>) (Dims a) (Dims b) = Dims $ M.filter (/= 0) $ M.unionWith (+) a b

instance Monoid Dims where
  mempty = Dims M.empty

class (Semigroup a) => Disjoin a where
  (</>) :: a -> a -> a

instance Disjoin Dims where
  (</>) a (Dims b) = a <> Dims (M.map negate b)

fundamentalDims :: Dim -> Dims
fundamentalDims dim@(Fundamental _) = Dims [(dim, 1)]
fundamentalDims (Derived _ (Dims m)) = M.foldlWithKey' reduceDims mempty m
  where
    reduceDims dims d n =
      let (Dims m') = fundamentalDims d in dims <> Dims (M.map (* n) m')

showDimsMap :: (Show a) => Map a Rational -> String
showDimsMap m =
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
