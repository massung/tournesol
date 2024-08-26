{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

module Tn.Script where

import qualified Data.Map.Strict as M
import Data.Symbol
import Text.Parsec hiding ((<|>))
import Tn.Conv
import Tn.Dims
import Tn.Units

data Script = Script
  { _dims :: M.Map Symbol (Dim, Maybe SourcePos),
    _units :: M.Map Symbol (Unit, Maybe SourcePos)
  }

instance Semigroup Script where
  (<>) a b =
    Script
      { _dims = M.union a._dims b._dims,
        _units = M.union a._units b._units
      }

instance Monoid Script where
  mempty =
    Script
      { _dims = mempty,
        _units = mempty
      }

_angle :: Dim
_angle = Fundamental "angle"

_area :: Dim
_area = Derived "area" $ Dims [(_length, 2)]

_capacitance :: Dim
_capacitance = Derived "capacitance" $ Dims [(_duration, 4), (_current, 2), (_mass, -1), (_length, -2)]

_charge :: Dim
_charge = Derived "charge" $ Dims [(_current, 1), (_duration, 1)]

_current :: Dim
_current = Fundamental "current"

_duration :: Dim
_duration = Fundamental "duration"

_energy :: Dim
_energy = Derived "energy" $ Dims [(_mass, 1), (_length, 2), (_duration, -2)]

_force :: Dim
_force = Derived "force" $ Dims [(_mass, 1), (_length, 1), (_duration, -2)]

_frequency :: Dim
_frequency = Derived "frequency" $ Dims [(_duration, -1)]

_length :: Dim
_length = Fundamental "length"

_mass :: Dim
_mass = Fundamental "mass"

_power :: Dim
_power = Derived "power" $ Dims [(_mass, 1), (_length, 2), (_duration, -3)]

_pressure :: Dim
_pressure = Derived "pressure" $ Dims [(_mass, 1), (_length, -1), (_duration, -2)]

_resistance :: Dim
_resistance = Derived "resistance" $ Dims [(_mass, 1), (_length, 2), (_duration, -3), (_current, -4)]

_speed :: Dim
_speed = Derived "speed" $ Dims [(_length, 1), (_duration, -1)]

_storage :: Dim
_storage = Fundamental "storage"

_temperature :: Dim
_temperature = Fundamental "temperature"

_voltage :: Dim
_voltage = Derived "voltage" $ Dims [(_mass, 1), (_length, 2), (_duration, -3), (_current, -1)]

_volume :: Dim
_volume = Derived "volume" $ Dims [(_length, 3)]

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

_pi :: Rational
_pi = approxRational (pi :: Double) 1e-20

defaultUnits :: [[Unit]]
defaultUnits =
  [ -- angle units
    [ Unit {_symbol = "rad", _name = "radians", _dim = _angle, _conv = Base},
      Unit {_symbol = "grad", _name = "gradians", _dim = _angle, _conv = Linear (200 / _pi)},
      Unit {_symbol = "deg", _name = "degrees", _dim = _angle, _conv = Linear (180 / _pi)},
      Unit {_symbol = "rev", _name = "revolutions", _dim = _angle, _conv = Linear ((1 % 2) / _pi)},
      Unit {_symbol = "turn", _name = "turns", _dim = _angle, _conv = Linear ((1 % 2) / _pi)}
    ],
    -- area units (imperial)
    [ Unit {_symbol = "ha", _name = "hectares", _dim = _area, _conv = Linear 1e-4},
      Unit {_symbol = "acre", _name = "acres", _dim = _area, _conv = Linear (78125 % 316160658)}
    ],
    -- duration units
    [ Unit {_symbol = "s", _name = "seconds", _dim = _duration, _conv = Base},
      Unit {_symbol = "min", _name = "minutes", _dim = _duration, _conv = Linear (1 % 60)},
      Unit {_symbol = "hr", _name = "hours", _dim = _duration, _conv = Linear (1 % 3600)},
      Unit {_symbol = "day", _name = "days", _dim = _duration, _conv = Linear (1 % 86400)}
    ],
    -- electrcal units
    siUnits Unit {_symbol = "F", _name = "farads", _dim = _capacitance, _conv = Base},
    siUnits Unit {_symbol = "C", _name = "coulombs", _dim = _charge, _conv = Base},
    siUnits Unit {_symbol = "A", _name = "amperes", _dim = _current, _conv = Base},
    siUnits Unit {_symbol = "O", _name = "ohms", _dim = _resistance, _conv = Base},
    siUnits Unit {_symbol = "V", _name = "volts", _dim = _voltage, _conv = Base},
    -- energy units
    siUnits Unit {_symbol = "J", _name = "joules", _dim = _energy, _conv = Base},
    siUnits Unit {_symbol = "eV", _name = "electron volts", _dim = _energy, _conv = Linear (5000000000000000000000000000 % 801088317)},
    -- energy units (imperial)
    [ Unit {_symbol = "BTU", _name = "British thermal units", _dim = _energy, _conv = Linear (50000000 % 52752792631)},
      Unit {_symbol = "thm", _name = "therms", _dim = _energy, _conv = Linear (1 % 105480400)}
    ],
    -- force units
    siUnits Unit {_symbol = "N", _name = "newtons", _dim = _force, _conv = Base},
    -- force units (imperial)
    [ Unit {_symbol = "lbf", _name = "pounds-force", _dim = _force, _conv = Linear (2000000000000 % 8896443230521)},
      Unit {_symbol = "pond", _name = "ponds", _dim = _force, _conv = Linear (20000000 % 1961333)}
    ],
    -- frequency units
    siUnits Unit {_symbol = "hz", _name = "hertz", _dim = _frequency, _conv = Base},
    -- length units
    siUnits Unit {_symbol = "m", _name = "meters", _dim = _length, _conv = Base},
    -- length units (imperial)
    [ Unit {_symbol = "mil", _name = "mils", _dim = _length, _conv = Linear 39370},
      Unit {_symbol = "in", _name = "inches", _dim = _length, _conv = Linear (5000 % 127)},
      Unit {_symbol = "h", _name = "hands", _dim = _length, _conv = Linear (3750 % 381)},
      Unit {_symbol = "ft", _name = "foots", _dim = _length, _conv = Linear (1250 % 381)},
      Unit {_symbol = "yd", _name = "yards", _dim = _length, _conv = Linear (1250 % 1143)},
      Unit {_symbol = "ftm", _name = "fathoms", _dim = _length, _conv = Linear (625 % 1143)},
      Unit {_symbol = "ch", _name = "chains", _dim = _length, _conv = Linear (625 % 12573)},
      Unit {_symbol = "fur", _name = "furlongs", _dim = _length, _conv = Linear (125 % 25146)},
      Unit {_symbol = "mi", _name = "miles", _dim = _length, _conv = Linear (125 % 201168)},
      Unit {_symbol = "lea", _name = "leagues", _dim = _length, _conv = Linear (125 % 603504)},
      Unit {_symbol = "cable", _name = "cables", _dim = _length, _conv = Linear (125 % 27432)},
      Unit {_symbol = "nmi", _name = "nautical miles", _dim = _length, _conv = Linear (1 % 1852)},
      Unit {_symbol = "link", _name = "links", _dim = _length, _conv = Linear (62500 % 12573)},
      Unit {_symbol = "rod", _name = "rods", _dim = _length, _conv = Linear (2500 % 12573)}
    ],
    -- length units (astronomical)
    [ Unit {_symbol = "pc", _name = "parsecs", _dim = _length, _conv = Linear (1 % 30856775814913670)},
      Unit {_symbol = "au", _name = "astronomical units", _dim = _length, _conv = Linear (1 % 149597870700)},
      Unit {_symbol = "ly", _name = "lightyears", _dim = _length, _conv = Linear (1 % 9460730472580800)}
    ],
    -- mass units
    siUnits Unit {_symbol = "g", _name = "grams", _dim = _mass, _conv = Base},
    -- mass units (imperial)
    [ Unit {_symbol = "oz", _name = "ounces", _dim = _mass, _conv = Linear (1600000000 % 45359237)},
      Unit {_symbol = "lb", _name = "pounds", _dim = _mass, _conv = Linear (100000000 % 45359237)},
      Unit {_symbol = "st", _name = "stones", _dim = _mass, _conv = Linear (50000000 % 317514659)},
      Unit {_symbol = "cwt", _name = "hundred weights", _dim = _mass, _conv = Linear (1000000 % 45359237)},
      Unit {_symbol = "t", _name = "tons", _dim = _mass, _conv = Linear (50000 % 45359237)}
    ],
    -- power units
    siUnits Unit {_symbol = "W", _name = "watts", _dim = _power, _conv = Base},
    -- power units (imperial)
    [ Unit {_symbol = "hp", _name = "horsepower", _dim = _power, _conv = Linear 0.001341}
    ],
    -- pressure units
    siUnits Unit {_symbol = "Pa", _name = "pascals", _dim = _pressure, _conv = Base},
    -- pressure units (imperial)
    [ Unit {_symbol = "psi", _name = "pounds-force per square inch", _dim = _pressure, _conv = Linear 145e-6},
      Unit {_symbol = "bar", _name = "bars", _dim = _pressure, _conv = Linear 1e-5}
    ],
    -- speed units
    [ Unit {_symbol = "kph", _name = "kilometers per hour", _dim = _speed, _conv = Linear (18 % 5)},
      Unit {_symbol = "kn", _name = "knots", _dim = _speed, _conv = Linear (3125 % 1397)},
      Unit {_symbol = "mph", _name = "miles per hour", _dim = _speed, _conv = Linear (900 % 463)}
    ],
    -- storage units
    storageUnits Unit {_symbol = "B", _name = "bytes", _dim = _storage, _conv = Base},
    storageUnits Unit {_symbol = "b", _name = "bits", _dim = _storage, _conv = Linear 8},
    -- temperature units
    [ Unit {_symbol = "Tc", _name = "degrees celcius", _dim = _temperature, _conv = Base},
      Unit {_symbol = "Tf", _name = "degrees fahrenheit", _dim = _temperature, _conv = Conv cToF fToC},
      Unit {_symbol = "Tk", _name = "degrees kelvin", _dim = _temperature, _conv = Conv cToK kToC},
      Unit {_symbol = "Tr", _name = "degrees rankine", _dim = _temperature, _conv = Conv cToR rToC}
    ],
    -- volume units
    siUnits Unit {_symbol = "L", _name = "liters", _dim = _volume, _conv = Linear 1000},
    -- volume units (imperial)
    [ Unit {_symbol = "tsp", _name = "teaspoons", _dim = _volume, _conv = Linear (96000000000000 % 473176473)},
      Unit {_symbol = "tbsp", _name = "tablespoons", _dim = _volume, _conv = Linear (32000000000000 % 473176473)},
      Unit {_symbol = "floz", _name = "floor ounces", _dim = _volume, _conv = Linear (16000000000000 % 473176473)},
      Unit {_symbol = "c", _name = "cups", _dim = _volume, _conv = Linear (2000000000000 % 473176473)},
      Unit {_symbol = "pt", _name = "pints", _dim = _volume, _conv = Linear (1000000000000 % 473176473)},
      Unit {_symbol = "qt", _name = "quarts", _dim = _volume, _conv = Linear (500000000000 % 473176473)},
      Unit {_symbol = "gal", _name = "gallons", _dim = _volume, _conv = Linear (125000000000 % 473176473)}
    ]
  ]
  where
    -- fahrenheit conversions
    cToF x = x * (9 % 5) + 32
    fToC x = (x - 32) * (5 % 9)

    -- kelvin conversions
    cToK x = x + 273.15
    kToC x = x - 273.15

    -- rankine conversions
    cToR x = x * (9 % 5) + 491.67
    rToC x = (x - 491.67) * (5 % 9)

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
declDefaultUnits scr = foldM decl scr $ concat defaultUnits
  where
    decl script u = declUnit u Nothing script

defaultScript :: Script
defaultScript = case foldM (&) mempty defaults of
  Left err -> error err
  Right script -> script
  where
    defaults :: [Script -> Either String Script]
    defaults =
      [ declDefaultDims,
        declDefaultUnits
      ]
