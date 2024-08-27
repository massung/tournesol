{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}

module Tn.Builtins where

import Tn.Conv
import Tn.Dims
import Tn.Units

--
-- dimensions
--

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

--
-- constants
--

_pi :: Rational
_pi = toRational (pi :: Double)

--
-- angle units
--

_rad :: Unit
_rad = Unit {_symbol = "rad", _name = "radians", _dim = _angle, _conv = Base}

_grad :: Unit
_grad = Unit {_symbol = "grad", _name = "gradians", _dim = _angle, _conv = Linear (200 / _pi)}

_deg :: Unit
_deg = Unit {_symbol = "deg", _name = "degrees", _dim = _angle, _conv = Linear (180 / _pi)}

_rev :: Unit
_rev = Unit {_symbol = "rev", _name = "revolutions", _dim = _angle, _conv = Linear ((1 % 2) / _pi)}

_turn :: Unit
_turn = Unit {_symbol = "turn", _name = "turns", _dim = _angle, _conv = Linear ((1 % 2) / _pi)}

--
-- area units
--

_ha :: Unit
_ha = Unit {_symbol = "ha", _name = "hectares", _dim = _area, _conv = Linear 1e-4}

_acre :: Unit
_acre = Unit {_symbol = "acre", _name = "acres", _dim = _area, _conv = Linear (78125 % 316160658)}

--
-- duration units
--

_s :: Unit
_s = Unit {_symbol = "s", _name = "seconds", _dim = _duration, _conv = Base}

_min :: Unit
_min = Unit {_symbol = "min", _name = "minutes", _dim = _duration, _conv = Linear (1 % 60)}

_hr :: Unit
_hr = Unit {_symbol = "hr", _name = "hours", _dim = _duration, _conv = Linear (1 % 3600)}

_day :: Unit
_day = Unit {_symbol = "day", _name = "days", _dim = _duration, _conv = Linear (1 % 86400)}

--
-- electrical units
--

_F :: Unit
_F = Unit {_symbol = "F", _name = "farads", _dim = _capacitance, _conv = Base}

_C :: Unit
_C = Unit {_symbol = "C", _name = "coulombs", _dim = _charge, _conv = Base}

_A :: Unit
_A = Unit {_symbol = "A", _name = "amperes", _dim = _current, _conv = Base}

_O :: Unit
_O = Unit {_symbol = "O", _name = "ohms", _dim = _resistance, _conv = Base}

_V :: Unit
_V = Unit {_symbol = "V", _name = "volts", _dim = _voltage, _conv = Base}

--
-- energy units
--

_J :: Unit
_J = Unit {_symbol = "J", _name = "joules", _dim = _energy, _conv = Base}

_eV :: Unit
_eV = Unit {_symbol = "eV", _name = "electron volts", _dim = _energy, _conv = Linear (5000000000000000000000000000 % 801088317)}

_BTU :: Unit
_BTU = Unit {_symbol = "BTU", _name = "British thermal units", _dim = _energy, _conv = Linear (50000000 % 52752792631)}

_thm :: Unit
_thm = Unit {_symbol = "thm", _name = "therms", _dim = _energy, _conv = Linear (1 % 105480400)}

--
-- force units
--

_N :: Unit
_N = Unit {_symbol = "N", _name = "newtons", _dim = _force, _conv = Base}

_lbf :: Unit
_lbf = Unit {_symbol = "lbf", _name = "pounds-force", _dim = _force, _conv = Linear (2000000000000 % 8896443230521)}

_pond :: Unit
_pond = Unit {_symbol = "pond", _name = "ponds", _dim = _force, _conv = Linear (20000000 % 1961333)}

--
-- frequency units
--

_hz :: Unit
_hz = Unit {_symbol = "hz", _name = "hertz", _dim = _frequency, _conv = Base}

--
-- length units
--

_m :: Unit
_m = Unit {_symbol = "m", _name = "meters", _dim = _length, _conv = Base}

_mil :: Unit
_mil = Unit {_symbol = "mil", _name = "mils", _dim = _length, _conv = Linear 39370}

_in :: Unit
_in = Unit {_symbol = "in", _name = "inches", _dim = _length, _conv = Linear (5000 % 127)}

_h :: Unit
_h = Unit {_symbol = "h", _name = "hands", _dim = _length, _conv = Linear (3750 % 381)}

_ft :: Unit
_ft = Unit {_symbol = "ft", _name = "foots", _dim = _length, _conv = Linear (1250 % 381)}

_yd :: Unit
_yd = Unit {_symbol = "yd", _name = "yards", _dim = _length, _conv = Linear (1250 % 1143)}

_ftm :: Unit
_ftm = Unit {_symbol = "ftm", _name = "fathoms", _dim = _length, _conv = Linear (625 % 1143)}

_ch :: Unit
_ch = Unit {_symbol = "ch", _name = "chains", _dim = _length, _conv = Linear (625 % 12573)}

_fur :: Unit
_fur = Unit {_symbol = "fur", _name = "furlongs", _dim = _length, _conv = Linear (125 % 25146)}

_mi :: Unit
_mi = Unit {_symbol = "mi", _name = "miles", _dim = _length, _conv = Linear (125 % 201168)}

_lea :: Unit
_lea = Unit {_symbol = "lea", _name = "leagues", _dim = _length, _conv = Linear (125 % 603504)}

_cable :: Unit
_cable = Unit {_symbol = "cable", _name = "cables", _dim = _length, _conv = Linear (125 % 27432)}

_nmi :: Unit
_nmi = Unit {_symbol = "nmi", _name = "nautical miles", _dim = _length, _conv = Linear (1 % 1852)}

_link :: Unit
_link = Unit {_symbol = "link", _name = "links", _dim = _length, _conv = Linear (62500 % 12573)}

_rod :: Unit
_rod = Unit {_symbol = "rod", _name = "rods", _dim = _length, _conv = Linear (2500 % 12573)}

--
-- length units (astronomical)
--

_pc :: Unit
_pc = Unit {_symbol = "pc", _name = "parsecs", _dim = _length, _conv = Linear (1 % 30856775814913670)}

_au :: Unit
_au = Unit {_symbol = "au", _name = "astronomical units", _dim = _length, _conv = Linear (1 % 149597870700)}

_ly :: Unit
_ly = Unit {_symbol = "ly", _name = "lightyears", _dim = _length, _conv = Linear (1 % 9460730472580800)}

--
-- mass units
--

_g :: Unit
_g = Unit {_symbol = "g", _name = "grams", _dim = _mass, _conv = Base}

_oz :: Unit
_oz = Unit {_symbol = "oz", _name = "ounces", _dim = _mass, _conv = Linear (1600000000 % 45359237)}

_lb :: Unit
_lb = Unit {_symbol = "lb", _name = "pounds", _dim = _mass, _conv = Linear (100000000 % 45359237)}

_st :: Unit
_st = Unit {_symbol = "st", _name = "stones", _dim = _mass, _conv = Linear (50000000 % 317514659)}

_cwt :: Unit
_cwt = Unit {_symbol = "cwt", _name = "hundred weights", _dim = _mass, _conv = Linear (1000000 % 45359237)}

_t :: Unit
_t = Unit {_symbol = "t", _name = "tons", _dim = _mass, _conv = Linear (50000 % 45359237)}

-- power units
_W :: Unit
_W = Unit {_symbol = "W", _name = "watts", _dim = _power, _conv = Base}

_hp :: Unit
_hp = Unit {_symbol = "hp", _name = "horsepower", _dim = _power, _conv = Linear 0.001341}

--
-- pressure units
--

_Pa :: Unit
_Pa = Unit {_symbol = "Pa", _name = "pascals", _dim = _pressure, _conv = Base}

_psi :: Unit
_psi = Unit {_symbol = "psi", _name = "pounds-force per square inch", _dim = _pressure, _conv = Linear 145e-6}

_bar :: Unit
_bar = Unit {_symbol = "bar", _name = "bars", _dim = _pressure, _conv = Linear 1e-5}

--
-- speed units
--

_kph :: Unit
_kph = Unit {_symbol = "kph", _name = "kilometers per hour", _dim = _speed, _conv = Linear (18 % 5)}

_kn :: Unit
_kn = Unit {_symbol = "kn", _name = "knots", _dim = _speed, _conv = Linear (3125 % 1397)}

_mph :: Unit
_mph = Unit {_symbol = "mph", _name = "miles per hour", _dim = _speed, _conv = Linear (900 % 463)}

--
-- storage units
--

_B :: Unit
_B = Unit {_symbol = "B", _name = "bytes", _dim = _storage, _conv = Base}

_b :: Unit
_b = Unit {_symbol = "b", _name = "bits", _dim = _storage, _conv = Linear 8}

--
-- temperature units
--

_Tc :: Unit
_Tc = Unit {_symbol = "Tc", _name = "degrees celcius", _dim = _temperature, _conv = Base}

_Tf :: Unit
_Tf = Unit {_symbol = "Tf", _name = "degrees fahrenheit", _dim = _temperature, _conv = Conv cToF fToC}
  where
    cToF x = x * (9 % 5) + 32
    fToC x = (x - 32) * (5 % 9)

_Tk :: Unit
_Tk = Unit {_symbol = "Tk", _name = "degrees kelvin", _dim = _temperature, _conv = Conv cToK kToC}
  where
    cToK x = x + 273.15
    kToC x = x - 273.15

_Tr :: Unit
_Tr = Unit {_symbol = "Tr", _name = "degrees rankine", _dim = _temperature, _conv = Conv cToR rToC}
  where
    cToR x = x * (9 % 5) + 491.67
    rToC x = (x - 491.67) * (5 % 9)

--
-- volume units
--

_L :: Unit
_L = Unit {_symbol = "L", _name = "liters", _dim = _volume, _conv = Linear 1000}

_tsp :: Unit
_tsp = Unit {_symbol = "tsp", _name = "teaspoons", _dim = _volume, _conv = Linear (96000000000000 % 473176473)}

_tbsp :: Unit
_tbsp = Unit {_symbol = "tbsp", _name = "tablespoons", _dim = _volume, _conv = Linear (32000000000000 % 473176473)}

_floz :: Unit
_floz = Unit {_symbol = "floz", _name = "floor ounces", _dim = _volume, _conv = Linear (16000000000000 % 473176473)}

_c :: Unit
_c = Unit {_symbol = "c", _name = "cups", _dim = _volume, _conv = Linear (2000000000000 % 473176473)}

_pt :: Unit
_pt = Unit {_symbol = "pt", _name = "pints", _dim = _volume, _conv = Linear (1000000000000 % 473176473)}

_qt :: Unit
_qt = Unit {_symbol = "qt", _name = "quarts", _dim = _volume, _conv = Linear (500000000000 % 473176473)}

_gal :: Unit
_gal = Unit {_symbol = "gal", _name = "gallons", _dim = _volume, _conv = Linear (125000000000 % 473176473)}
