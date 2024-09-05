module Tn.Conv where

import Algebra.Graph.Labelled.AdjacencyMap
import qualified Data.Set as S
import Tn.System
import Tn.Unit

-- labeled graph, units are vertices and conversion functions are edges
type ConvGraph = AdjacencyMap (Maybe Conv) Unit

-- Conversion edge data
--
-- Each conversion edge consists of the destination vertex (Unit),
-- an exponent ratio (from % to), and a function that performs the
-- conversion (\from -> to).
--
-- For example, the conversions from m -> cm and cm^3 -> L are:
--
--   Conv (m, 1 % 1) (* 100)
--   Conv (L, 3 % 1) (/ 1000)
data Conv = Conv (Unit, Rational) (Rational -> Rational)

instance Eq Conv where
  (==) (Conv (a, _) _) (Conv (b, _) _) = a == b

instance Ord Conv where
  (<=) (Conv (a, _) _) (Conv (b, _) _) = a <= b

instance Show Conv where
  show (Conv (a, n) _) =
    if denominator n == 1
      then printf "Conv to %s" (show a)
      else printf "Conv to %s^%d" (show a) (denominator n)

instance Semigroup Conv where
  (<>) (Conv (to, nTo) f) (Conv (_, _) g) = Conv (to, nTo) (f . g)

-- apply a conversion function
applyConv :: Rational -> Conv -> Rational
applyConv n (Conv _ f) = f n

-- create a pair of linear conversions
linearConvs :: Unit -> (Unit, Rational) -> Rational -> ConvGraph
linearConvs from (to, n) r =
  let a = edge (Just $ Conv (to, n) (/ r)) from to
      b = edge (Just $ Conv (from, recip n) (* r)) to from
   in overlay a b

derivedConvs :: [(String, String, Rational)] -> Unit -> ConvGraph
derivedConvs prefixes u = overlays $ fmap convs prefixes
  where
    convs (_, p, r) = linearConvs u (unitWithPrefix u p, 1) r

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
findConv :: (Unit, Int) -> Unit -> ConvGraph -> Maybe Conv
findConv (from, n) to gr = fmap (foldl1' (<>)) path
  where
    path = bfs [[Conv (from, toRational n) id]] (S.singleton from) to gr

-- breadth first path search
bfs :: [[Conv]] -> Set Unit -> Unit -> ConvGraph -> Maybe [Conv]
bfs [] _ _ _ = Nothing
bfs q s to gr = find ((== to) . fst . goal) q <|> bfs q' s' to gr
  where
    goal :: [Conv] -> (Unit, Rational)
    goal [] = error "Unreachable; empty search path added to BFS!"
    goal (Conv u _ : _) = u

    -- Return all possible branches of this path. This takes the conversion
    -- exponent into account with the supplied exponent of the original unit.
    --
    -- For example, when converting from cm -> L, it's only possible to
    -- convert in multiples of cm^3. Example conversions:
    --
    --   L -> cm^3 = Conv (cm, 1 % 3) (* 1000)
    --   cm^3 -> L = Conv (L, 3 % 1) (/ 1000)

    walk :: [Conv] -> [[Conv]]
    walk path =
      let (from, n) = goal path
          steps = S.toList $ S.difference (postSet from gr) s
          convs = [edgeLabel from step gr | step <- steps]
       in [r' c n : path | Just c@(Conv (_, e) _) <- convs, numerator n `mod` numerator e == 0]

    -- next queue with updated paths
    q' = concatMap walk q
    s' = foldl (flip S.insert) s [fst $ goal p | p <- q']

    -- exponentiate the conversion function
    r' (Conv (u, e) r) n =
      let i = numerator n `div` numerator e
          f = foldl' (.) r $ replicate (fromInteger $ i - 1) r
       in Conv (u, n / e) $ if n >= 0 then f else (/ f 1)
