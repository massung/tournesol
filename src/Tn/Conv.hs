module Tn.Conv where

import Algebra.Graph.Labelled.AdjacencyMap
import qualified Data.Set as S
import Tn.System
import Tn.Unit

-- labeled graph, units are vertices and conversion functions are edges
type ConvGraph = AdjacencyMap (Maybe Conv) Unit

-- Conversion edge data
--
-- Each conversion edge consists of the destination vertex (Unit)
-- and a function that performs the conversion (from -> to).
--
-- For example, the conversions from m -> cm and cm -> m are:
--
--   Conv m (* 100)
--   Conv cm (/ 1000)
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
linearConvs from to r =
  let a = edge (Just $ Conv to (/ r)) from to
      b = edge (Just $ Conv from (* r)) to from
   in overlay a b

derivedConvs :: [(String, String, Rational)] -> Unit -> ConvGraph
derivedConvs prefixes u = overlays $ fmap convs prefixes
  where
    convs (_, p, r) = linearConvs u (unitWithPrefix u p r) r

-- return a metric conversion graph given the fundamental unit
siConvs :: Unit -> ConvGraph
siConvs = derivedConvs siPrefixes

-- return a binary conversion graph given the fundamental unit
binaryConvs :: Unit -> ConvGraph
binaryConvs = derivedConvs binaryPrefixes

-- returns an empty conversion graph
mkConvGraph :: [ConvGraph] -> ConvGraph
mkConvGraph = overlays

-- search the graph and folds the conversion path into a single conversion
findConv :: (Unit, Int) -> Unit -> ConvGraph -> Maybe Conv
findConv from to gr = foldl1' (<>) <$> findConvPath from to gr

-- searches the graph and returns a path of conversions
findConvPath :: (Unit, Int) -> Unit -> ConvGraph -> Maybe [Conv]
findConvPath (from, e) = bfs [[Conv from id]] (S.singleton from) e

-- breadth first path search
bfs :: [[Conv]] -> Set Unit -> Int -> Unit -> ConvGraph -> Maybe [Conv]
bfs [] _ _ _ _ = Nothing
bfs q s e to gr = find ((== to) . goal) q <|> bfs q' s' e to gr
  where
    goal :: [Conv] -> Unit
    goal [] = error "Unreachable; empty search path added to BFS!"
    goal (Conv u _ : _) = u

    walk :: [Conv] -> [[Conv]]
    walk path =
      let from = goal path
          steps = S.toList $ S.difference (postSet from gr) s
          convs = [edgeLabel from step gr | step <- steps]
       in [expConv c : path | Just c <- convs]

    expConv :: Conv -> Conv
    expConv (Conv u f) =
      let f' = foldl' (.) f $ replicate (abs e - 1) f
       in Conv u $ if e >= 0 then f' else (/ f' 1)

    -- next queue with updated paths
    q' = concatMap walk q
    s' = foldl (flip S.insert) s [goal p | p <- q']
