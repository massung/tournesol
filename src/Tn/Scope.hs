{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

module Tn.Scope where

import qualified Data.Map.Strict as M
import Data.Symbol
import Text.Parsec hiding ((<|>))
import Tn.Builtins
import Tn.Conv
import Tn.Dims
import Tn.Function
import Tn.Scalar
import Tn.Units

data Scope = Scope
  { _dims :: M.Map Symbol (Dim, Maybe SourcePos),
    _units :: M.Map Symbol (Unit, Maybe SourcePos),
    _funcs :: M.Map Symbol (Function, Maybe SourcePos),
    _locals :: M.Map Symbol (Scalar, Maybe SourcePos)
  }

-- scopes are right-biased
instance Semigroup Scope where
  (<>) a b =
    Scope
      { _dims = M.union b._dims a._dims,
        _units = M.union b._units a._units,
        _funcs = M.union b._funcs a._funcs,
        _locals = M.union b._locals a._locals
      }

instance Monoid Scope where
  mempty =
    Scope
      { _dims = mempty,
        _units = mempty,
        _funcs = mempty,
        _locals = mempty
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

declDim :: Symbol -> Dim -> Maybe SourcePos -> Scope -> Either String Scope
declDim s dim pos scr =
  case M.lookup s scr._dims of
    Just (_, Just orgPos) -> Left $ "dimension already defined at " ++ show orgPos
    Just (_, _) -> Left "dimension already defined"
    _ -> Right scr {_dims = M.insert s (dim, pos) scr._dims}

declUnit :: Unit -> Maybe SourcePos -> Scope -> Either String Scope
declUnit u pos scr =
  case M.lookup u._symbol scr._units of
    Just (_, Just orgPos) -> Left $ "units already defined at " ++ show orgPos
    Just (_, _) -> Left "units already defined"
    _ -> Right scr {_units = M.insert u._symbol (u, pos) scr._units}

declFunction :: Symbol -> (Function, Maybe SourcePos) -> Scope -> Either String Scope
declFunction s func scr =
  if isJust $ M.lookup s scr._funcs
    then Left "function already defined"
    else Right scr {_funcs = M.insert s func scr._funcs}

declUnits :: [Unit] -> Maybe SourcePos -> Scope -> Either String Scope
declUnits us pos scr = foldM decl scr us
  where
    decl s u = declUnit u pos s

declDefaultDims :: Scope -> Either String Scope
declDefaultDims scr = foldM decl scr defaultDims
  where
    decl script dim@(Fundamental s) = declDim s dim Nothing script
    decl script dim@(Derived s _) = declDim s dim Nothing script

declDefaultUnits :: Scope -> Either String Scope
declDefaultUnits scr = foldM decl scr defaultUnits
  where
    decl script u = declUnit u Nothing script

declDefaultFunctions :: Scope -> Either String Scope
declDefaultFunctions scr = foldM decl scr defaultFunctions
  where
    decl script (name, func) = declFunction name (func, Nothing) script

defaultScope :: Scope
defaultScope = case foldM (&) mempty defaults of
  Left err -> error err
  Right script -> script
  where
    defaults :: [Scope -> Either String Scope]
    defaults =
      [ declDefaultDims,
        declDefaultUnits,
        declDefaultFunctions
      ]

baseUnits :: Scope -> [(Unit, Maybe SourcePos)]
baseUnits scope = M.elems $ M.filter (isBase . _conv . fst) scope._units

baseDims :: Scope -> [Dim]
baseDims scope = map fst $ M.elems scope._dims
