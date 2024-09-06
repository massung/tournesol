{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

module Tn.Scope where

import qualified Algebra.Graph.Labelled.AdjacencyMap as G
import qualified Data.Map.Strict as M
import Tn.Conv
import Tn.Scalar
import Tn.Symbol
import Tn.Unit

data Scope = Scope
  { _convs :: ConvGraph,
    _dims :: Map Symbol Base,
    _units :: Map Symbol Unit,
    _constants :: Map Symbol Scalar,
    _locals :: Map Symbol Scalar,
    _ans :: Scalar,
    _epsilon :: Double
  }

-- scopes are right-biased
instance Semigroup Scope where
  (<>) a b =
    Scope
      { _convs = G.overlay a._convs b._convs,
        _dims = a._dims <> b._dims,
        _units = a._units <> b._units,
        _constants = a._constants <> b._constants,
        _locals = a._locals <> b._locals,
        _ans = b._ans,
        _epsilon = b._epsilon
      }

instance Monoid Scope where
  mempty =
    Scope
      { _convs = G.empty,
        _dims = mempty,
        _units = mempty,
        _constants = mempty,
        _locals = mempty,
        _ans = 0,
        _epsilon = 1e-10
      }

declDim :: Symbol -> Scope -> Either String Scope
declDim d scope =
  if M.member d scope._dims
    then Left "dimension already defined"
    else Right scope {_dims = M.insert d (Base d) scope._dims}

declUnit :: Unit -> Scope -> Either String Scope
declUnit u@(Unit s _) scope =
  if M.member s scope._units
    then Left "unit already defined"
    else Right scope {_units = M.insert s u scope._units}

declConvs :: ConvGraph -> Scope -> Scope
declConvs g scope =
  scope
    { _convs = G.overlay scope._convs g,
      _units = M.union scope._units $ fromList units'
    }
  where
    units' = [(s, u) | u@(Unit s _) <- G.vertexList g]
