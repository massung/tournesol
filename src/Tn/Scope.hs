{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

module Tn.Scope where

import qualified Algebra.Graph.Labelled.AdjacencyMap as G
import qualified Data.Map.Strict as M
import Tn.Context
import Tn.Conv
import Tn.Function
import Tn.Scalar
import Tn.Symbol
import Tn.Unit

data Scope = Scope
  { _convs :: ConvGraph,
    _dims :: Map Symbol Base,
    _units :: Map Symbol Unit,
    _functions :: Map Symbol Function,
    _globals :: Map Symbol Scalar
  }

-- scopes are right-biased
instance Semigroup Scope where
  (<>) a b =
    Scope
      { _convs = G.overlay a._convs b._convs,
        _dims = a._dims <> b._dims,
        _units = a._units <> b._units,
        _functions = a._functions <> b._functions,
        _globals = a._globals <> b._globals
      }

instance Monoid Scope where
  mempty =
    Scope
      { _convs = G.empty,
        _dims = mempty,
        _units = mempty,
        _functions = mempty,
        _globals = mempty
      }

mkContext :: Scope -> Context
mkContext scope = Context scope._convs []

declDim :: Symbol -> Scope -> Either String Scope
declDim d scope =
  if M.member d scope._dims
    then Left "dimension already defined"
    else Right scope {_dims = M.insert d (Base d) scope._dims}

declConst :: Symbol -> Scalar -> Scope -> Either String Scope
declConst s x scope =
  if M.member s scope._globals
    then Left "constant already defined"
    else Right scope {_globals = M.insert s x scope._globals}

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

declFunction :: Symbol -> Function -> Scope -> Either String Scope
declFunction name f scope =
  if M.member name scope._functions
    then Left "function already defined"
    else Right scope {_functions = M.insert name f scope._functions}
