module Tn.Conv where

import Algebra.Graph.Labelled.AdjacencyMap
import qualified Data.Set as S
import Tn.System
import Tn.Unit
import Prelude hiding ((\\))

-- labeled graph, units are vertices and conversion functions are edges
type ConvGraph = AdjacencyMap (Maybe Conv) Unit

-- conversion function to Unit
data Conv = Conv Unit (Rational -> Rational)

instance Eq Conv where
  (==) (Conv a _) (Conv b _) = a == b

instance Ord Conv where
  (<=) (Conv a _) (Conv b _) = a <= b

instance Show Conv where
  show (Conv a _) = printf "Conv to %s" (show a)

instance Semigroup Conv where
  (<>) (Conv to f) (Conv _ g) = Conv to (f . g)

-- apply a conversion function
applyConv :: Rational -> Conv -> Rational
applyConv n (Conv _ f) = f n

-- create a pair of linear conversions
linearConvs :: Unit -> Unit -> Rational -> ConvGraph
linearConvs from to n =
  let a = edge (Just $ Conv to (/ n)) from to
      b = edge (Just $ Conv from (* n)) to from
   in overlay a b

derivedConvs :: [(String, String, Rational)] -> Unit -> ConvGraph
derivedConvs prefixes u = overlays $ fmap convs prefixes
  where
    convs (_, p, r) = linearConvs u (unitWithPrefix u p) r

-- return a metric conversion graph given the fundamental unit
siConvs :: Unit -> ConvGraph
siConvs = derivedConvs siPrefixes

-- return a binary conversion graph given the fundamental unit
binaryConvs :: Unit -> ConvGraph
binaryConvs = derivedConvs binaryPrefixes

-- returns an empty conversion graph
mkConvGraph :: [ConvGraph] -> ConvGraph
mkConvGraph = overlays

-- search the graph for a valid conversion
findConv :: Unit -> Unit -> ConvGraph -> Maybe Conv
findConv from to gr = fmap (foldl1' (<>)) path
  where
    path = bfs [[Conv from id]] (S.singleton from) to gr

-- breadth first path search
bfs :: [[Conv]] -> Set Unit -> Unit -> ConvGraph -> Maybe [Conv]
bfs [] _ _ _ = Nothing
bfs q s to gr = find ((== to) . goal) q <|> bfs q' s' to gr
  where
    goal :: [Conv] -> Unit
    goal path = let (Conv u _) = head path in u

    -- return all possible branches of this path
    walk :: [Conv] -> [[Conv]]
    walk path =
      let from = goal path
          steps = S.toList $ S.difference (postSet from gr) s
       in [fromJust (edgeLabel from step gr) : path | step <- steps]

    -- next queue with updated paths
    q' = concatMap walk q
    s' = foldl (flip S.insert) s [goal p | p <- q']

-- exponentiate a conversion function
(^.^) :: Conv -> Int -> Conv
(^.^) (Conv to f) n =
  if n >= 0
    then Conv to g
    else Conv to (/ g 1)
  where
    g = foldl' (.) id $ replicate (abs n) f
