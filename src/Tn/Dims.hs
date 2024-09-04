{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE TypeFamilies #-}

module Tn.Dims where

import qualified Data.Map.Strict as M
import Data.Tuple.Extra

newtype Dims a = Dims (M.Map a Int)
  deriving (Eq)

instance (Ord a) => IsList (Dims a) where
  type Item (Dims a) = (a, Int)

  -- convert from [(dim, n)] to Dims
  fromList = Dims . fromList

  -- convert from dimensions to [(dim, n)] list
  toList (Dims a) = toList a

instance (Ord a) => Semigroup (Dims a) where
  (<>) (Dims a) (Dims b) = Dims $ M.filter (/= 0) $ M.unionWith (+) a b

instance (Ord a) => Monoid (Dims a) where
  mempty = Dims M.empty

instance (Show a) => Show (Dims a) where
  show (Dims m) =
    let (num, den) = M.partition (> 0) m
     in if
          | null num -> show' den
          | null den -> show' num
          | otherwise -> show' num ++ "/" ++ show' (M.map abs den)
    where
      show' dims = unwords [showExp u | u <- M.toList dims]

      -- show a single dimension with optional exponent
      showExp (u, 1) = show u
      showExp (u, n) = printf "%s^%d" (show u) n

-- reciprocal of (<>)
(</>) :: (Ord a) => Dims a -> Dims a -> Dims a
(</>) a b = a <> recipDims b

-- define a singleton dimension
singleton :: a -> Dims a
singleton a = Dims $ M.singleton a 1

-- return the unique keys of a dimensions map
dimsKeys :: Dims a -> [a]
dimsKeys (Dims m) = M.keys m

-- return the exponent for a given key
dimExponent :: (Ord a) => a -> Dims a -> Maybe Int
dimExponent a (Dims m) = M.lookup a m

-- raise dimension exponents to power
(*^) :: (Ord a) => Dims a -> Int -> Dims a
(*^) _ 0 = mempty
(*^) dims 1 = dims
(*^) (Dims m) n = Dims $ M.map (* n) m

-- true if the dimensions are empty
nullDims :: Dims a -> Bool
nullDims (Dims m) = M.null m

-- return the reciprocal dimensions
recipDims :: Dims a -> Dims a
recipDims = mapDims negate

-- map dimensions, returning
mapDims :: (Int -> Int) -> Dims a -> Dims a
mapDims f (Dims m) = Dims $ M.map f m

-- fold over the dimensions
foldDims :: (b -> a -> Int -> b) -> b -> Dims a -> b
foldDims f i (Dims m) = M.foldlWithKey' f i m

-- split dimensions into numerator and denominator
partitionDims :: Dims a -> (Dims a, Dims a)
partitionDims (Dims m) = both Dims $ M.partition (>= 0) m
