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
  (==) (Derived a _) (Derived b _) = a == b
  (==) _ _ = False

instance Ord Dim where
  compare (Fundamental a) (Fundamental b) = compare a b
  compare (Derived a _) (Derived b _) = compare a b
  compare (Fundamental a) (Derived b _) = compare a b
  compare (Derived a _) (Fundamental b) = compare a b

-- dimension : exponent
type DimMap = M.Map Dim Rational

newtype Dims = Dims DimMap
  deriving (Eq, Show)

instance Semigroup Dims where
  (<>) (Dims a) (Dims b) = Dims $ M.filter (/= 0) $ M.unionWith (+) a b

instance Monoid Dims where
  mempty = Dims M.empty

fundamentalDims :: Dim -> Dims
fundamentalDims dim@(Fundamental _) = Dims [(dim, 1)]
fundamentalDims (Derived _ (Dims m)) = M.foldlWithKey' reduceDims mempty m
  where
    reduceDims dims d n =
      let (Dims m') = fundamentalDims d in dims <> Dims (M.map (* n) m')
