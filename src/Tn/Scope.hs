{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

module Tn.Scope where

import qualified Algebra.Graph.Labelled.AdjacencyMap as G
import qualified Data.Map.Strict as M
import Tn.Symbol
import Tn.Builtins
import Tn.Conv
import Tn.Scalar
import Tn.Unit

data Scope = Scope
  { _convs :: ConvGraph,
    _dims :: Map Symbol Base,
    _units :: Map Symbol Unit,
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
      _locals = mempty,
      _ans = 0,
      _epsilon = 0.0
    }

defaultDims :: [Base]
defaultDims =
  [ _angle,
    _area,
    _capacitance,
    _charge,
    _current,
    _energy,
    _force,
    _frequency,
    _length,
    _mass,
    _power,
    _pressure,
    _resistance,
    _speed,
    _storage,
    _temperature,
    _time,
    _voltage,
    _volume
  ]

fundamentalDims :: [Base]
fundamentalDims = [dim | dim@(Base _) <- defaultDims]

metricConvs :: ConvGraph
metricConvs =
  G.overlays
    [ -- electrical units
      siConvs _A,
      siConvs _C,
      siConvs _F,
      siConvs _O,
      siConvs _V,
      -- energy units
      siConvs _J,
      siConvs _eV,
      -- force units
      siConvs _N,
      -- frequency units
      siConvs _hz,
      -- length units
      siConvs _m,
      -- mass units
      siConvs _g,
      -- power units
      siConvs _W,
      -- pressure units
      siConvs _Pa,
      -- volume units
      siConvs _L
    ]

imperialConvs :: ConvGraph
imperialConvs =
  G.overlays
    [ -- length units (imperial)
      linearConvs _ft _m $ 1250 % 381,
      linearConvs _ft _in $ 1 % 12,
      linearConvs _ft _yd $ 3 % 1,
      linearConvs _ft _mi $ 5280 % 1
    ]


  -- [ -- area units (imperial)
  --   [_ha, _acre],
  --   -- energy units (imperial)
  --   [_BTU, _thm],
  --   -- force units (imperial)
  --   [_lbf, _pond],
  --   -- length units (imperial)
  --   [_mil, _in, _h, _ft, _yd, _ftm, _ch, _fur, _mi, _lea, _cable, _nmi, _link, _rod],
  --   -- mass units (imperial)
  --   [_oz, _lb, _st, _cwt, _t],
  --   -- power units (imperial)
  --   [_hp],
  --   -- pressure units (imperial)
  --   [_psi, _bar],
  --   -- volume units (imperial)
  --   [_tsp, _tbsp, _floz, _c, _pt, _qt, _gal]
  -- ]

angleConvs :: ConvGraph
angleConvs =
  G.overlays
    [ linearConvs _rad _deg $ toRational (pi / 180.0 :: Double),
      linearConvs _grad _deg $ 400 % 360,
      linearConvs _rev _deg $ 1 % 360,
      linearConvs _turn _deg $ 1 % 360
    ]

astronomicalConvs :: ConvGraph
astronomicalConvs =
  G.overlays
    [ linearConvs _m _au $ 1 % 149597870700,
      linearConvs _m _ly $ 1 % 94607304725808000,
      linearConvs _au _pc $ 1 % 206265
    ]

durationConvs :: ConvGraph
durationConvs =
  G.overlays
    [ linearConvs _s _min $ 60 % 1,
      linearConvs _min _hr $ 60  % 1,
      linearConvs _hr _day $ 24 % 1
    ]

-- speedUnits :: [[Unit]]
-- speedUnits = [[_kph, _kn, _mph]]

diskConvs :: ConvGraph
diskConvs =
  G.overlays
    [ storageConvs _B,
      storageConvs _b
    ]

-- temperatureUnits :: [[Unit]]
-- temperatureUnits = [[_Tc, _Tf, _Tk, _Tr]]

defaultConvs :: ConvGraph
defaultConvs =
  G.overlays
    [ angleConvs,
      astronomicalConvs,
      diskConvs,
      durationConvs,
      imperialConvs,
      metricConvs
      --speedConvs,
      --temperatureConvs
    ]

defaultScope :: Scope
defaultScope =
  mempty
    { _convs=defaultConvs,
      _dims=M.fromList dims,
      _units=M.fromList units
    }
  where
    units = [(s, u) | u@(Unit s _) <- G.vertexList defaultConvs]
    dims = [(s, d) | d@(Base s) <- fundamentalDims]

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
