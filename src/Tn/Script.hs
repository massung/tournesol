{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

module Tn.Script where

import qualified Data.Map.Strict as M
import Data.Symbol
import Text.Parsec hiding ((<|>))
import Tn.Builtins
import Tn.Dims
import Tn.Function
import Tn.Units

data Script = Script
  { _dims :: M.Map Symbol (Dim, Maybe SourcePos),
    _units :: M.Map Symbol (Unit, Maybe SourcePos),
    _funcs :: M.Map Symbol (Function, Maybe SourcePos)
  }

instance Semigroup Script where
  (<>) a b =
    Script
      { _dims = M.union a._dims b._dims,
        _units = M.union a._units b._units,
        _funcs = M.union a._funcs b._funcs
      }

instance Monoid Script where
  mempty =
    Script
      { _dims = mempty,
        _units = mempty,
        _funcs = mempty
      }

defaultDims :: [Dim]
defaultDims =
  [ _angle,
    _area,
    _capacitance,
    _charge,
    _current,
    _duration,
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
    _voltage,
    _volume
  ]

metricUnits :: [[Unit]]
metricUnits =
  [ -- electrcal units
    siUnits _A,
    siUnits _C,
    siUnits _F,
    siUnits _O,
    siUnits _V,
    -- energy units
    siUnits _J,
    siUnits _eV,
    -- force units
    siUnits _N,
    -- frequency units
    siUnits _hz,
    -- length units
    siUnits _m,
    -- mass units
    siUnits _g,
    -- power units
    siUnits _W,
    -- pressure units
    siUnits _Pa,
    -- volume units
    siUnits _L
  ]

imperialUnits :: [[Unit]]
imperialUnits =
  [ -- area units (imperial)
    [_ha, _acre],
    -- energy units (imperial)
    [_BTU, _thm],
    -- force units (imperial)
    [_lbf, _pond],
    -- length units (imperial)
    [_mil, _in, _h, _ft, _yd, _ftm, _ch, _fur, _mi, _lea, _cable, _nmi, _link, _rod],
    -- mass units (imperial)
    [_oz, _lb, _st, _cwt, _t],
    -- power units (imperial)
    [_hp],
    -- pressure units (imperial)
    [_psi, _bar],
    -- volume units (imperial)
    [_tsp, _tbsp, _floz, _c, _pt, _qt, _gal]
  ]

angleUnits :: [[Unit]]
angleUnits = [[_rad, _grad, _deg, _rev, _turn]]

astronomicalUnits :: [[Unit]]
astronomicalUnits = [[_pc, _au, _ly]]

durationUnits :: [[Unit]]
durationUnits = [[_s, _min, _hr, _day]]

speedUnits :: [[Unit]]
speedUnits = [[_kph, _kn, _mph]]

diskUnits :: [[Unit]]
diskUnits =
  [ storageUnits _B,
    storageUnits _b
  ]

temperatureUnits :: [[Unit]]
temperatureUnits = [[_Tc, _Tf, _Tk, _Tr]]

defaultUnits :: [Unit]
defaultUnits =
  concat
    $ mconcat
      [ angleUnits,
        astronomicalUnits,
        diskUnits,
        durationUnits,
        imperialUnits,
        metricUnits,
        speedUnits,
        temperatureUnits
      ]

declDim :: Symbol -> (Dim, Maybe SourcePos) -> Script -> Either String Script
declDim s dim scr =
  if isJust $ M.lookup s scr._dims
    then Left "dimension already defined"
    else Right scr {_dims = M.insert s dim scr._dims}

declUnit :: Unit -> Maybe SourcePos -> Script -> Either String Script
declUnit u pos scr =
  if isJust $ M.lookup u._symbol scr._units
    then Left "unit already defined"
    else Right scr {_units = M.insert u._symbol (u, pos) scr._units}

declFunction :: Symbol -> (Function, Maybe SourcePos) -> Script -> Either String Script
declFunction s func scr =
  if isJust $ M.lookup s scr._funcs
    then Left "function already defined"
    else Right scr {_funcs = M.insert s func scr._funcs}

declUnits :: [Unit] -> Maybe SourcePos -> Script -> Either String Script
declUnits us pos scr = foldM decl scr us
  where
    decl s u = declUnit u pos s

declDefaultDims :: Script -> Either String Script
declDefaultDims scr = foldM decl scr defaultDims
  where
    decl script dim@(Fundamental s) = declDim s (dim, Nothing) script
    decl script dim@(Derived s _) = declDim s (dim, Nothing) script

declDefaultUnits :: Script -> Either String Script
declDefaultUnits scr = foldM decl scr defaultUnits
  where
    decl script u = declUnit u Nothing script

declDefaultFunctions :: Script -> Either String Script
declDefaultFunctions scr = foldM decl scr defaultFunctions
  where
    decl script (name, func) = declFunction name (func, Nothing) script

defaultScript :: Script
defaultScript = case foldM (&) mempty defaults of
  Left err -> error err
  Right script -> script
  where
    defaults :: [Script -> Either String Script]
    defaults =
      [ declDefaultDims,
        declDefaultUnits,
        declDefaultFunctions
      ]
