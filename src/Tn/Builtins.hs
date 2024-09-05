{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}

module Tn.Builtins where

import Tn.Unit

--
-- fundamental dimensions
--

_angle :: Base
_angle = Base "angle"

_current :: Base
_current = Base "current"

_length :: Base
_length = Base "length"

_mass :: Base
_mass = Base "mass"

_storage :: Base
_storage = Base "storage"

_temperature :: Base
_temperature = Base "temperature"

_time :: Base
_time = Base "time"

--
-- derived units
--

_area :: Base
_area = Derived [(_ft, 2)]

_capacitance :: Base
_capacitance = Derived [(_s, 4), (_A, 2), (_kg, -1), (_m, -2)]

_charge :: Base
_charge = Derived [(_A, 1), (_s, 1)]

_energy :: Base
_energy = Derived [(_kg, 1), (_m, 2), (_s, -2)]

_force :: Base
_force = Derived [(_kg, 1), (_m, 1), (_s, -2)]

_frequency :: Base
_frequency = Derived [(_s, -1)]

_momentum :: Base
_momentum = Derived [(_kg, 1), (_m, 1), (_s, -1)]

_power :: Base
_power = Derived [(_kg, 1), (_m, 2), (_s, -3)]

_pressure :: Base
_pressure = Derived [(_kg, 1), (_m, -1), (_s, -2)]

_resistance :: Base
_resistance = Derived [(_kg, 1), (_m, 2), (_s, -3), (_A, -4)]

_speed :: Base
_speed = Derived [(_m, 1), (_s, -1)]

_voltage :: Base
_voltage = Derived [(_kg, 1), (_m, 2), (_s, -3), (_A, -1)]

_volume :: Base
_volume = Derived [(_cm, 3)]

--
-- angle units
--

_rad :: Unit
_rad = Unit "rad" _angle

_grad :: Unit
_grad = Unit "grad" _angle

_deg :: Unit
_deg = Unit "deg" _angle

_rev :: Unit
_rev = Unit "rev" _angle

_turn :: Unit
_turn = Unit "turn" _angle

--
-- area units
--

_ha :: Unit
_ha = Unit "ha" _area

_acre :: Unit
_acre = Unit "acre" _area

--
-- duration units
--

_s :: Unit
_s = Unit "s" _time

_min :: Unit
_min = Unit "min" _time

_hr :: Unit
_hr = Unit "hr" _time

_day :: Unit
_day = Unit "day" _time

--
-- electrical units
--

_F :: Unit
_F = Unit "F" _capacitance

_C :: Unit
_C = Unit "C" _charge

_A :: Unit
_A = Unit "A" _current

_O :: Unit
_O = Unit "O" _resistance

_V :: Unit
_V = Unit "V" _voltage

--
-- energy units
--

_J :: Unit
_J = Unit "J" _energy

_eV :: Unit
_eV = Unit "eV" _energy

_BTU :: Unit
_BTU = Unit "BTU" _energy

_thm :: Unit
_thm = Unit "thm" _energy

--
-- force units
--

_N :: Unit
_N = Unit "N" _force

_lbf :: Unit
_lbf = Unit "lbf" _force

_pond :: Unit
_pond = Unit "pond" _force

--
-- frequency units
--

_hz :: Unit
_hz = Unit "hz" _frequency

--
-- length units
--

_m :: Unit
_m = Unit "m" _length

_cm :: Unit
_cm = Unit "cm" _length

_mil :: Unit
_mil = Unit "mil" _length

_in :: Unit
_in = Unit "in" _length

_h :: Unit
_h = Unit "h" _length

_ft :: Unit
_ft = Unit "ft" _length

_yd :: Unit
_yd = Unit "yd" _length

_ftm :: Unit
_ftm = Unit "ftm" _length

_ch :: Unit
_ch = Unit "chain" _length

_fur :: Unit
_fur = Unit "fur" _length

_mi :: Unit
_mi = Unit "mi" _length

_lea :: Unit
_lea = Unit "lea" _length

_cable :: Unit
_cable = Unit "cable" _length

_nmi :: Unit
_nmi = Unit "nmi" _length

_link :: Unit
_link = Unit "link" _length

_rod :: Unit
_rod = Unit "rod" _length

--
-- length units (astronomical)
--

_pc :: Unit
_pc = Unit "pc" _length

_au :: Unit
_au = Unit "au" _length

_ly :: Unit
_ly = Unit "ly" _length

--
-- mass units
--

_g :: Unit
_g = Unit "g" _mass

_kg :: Unit
_kg = Unit "kg" _mass

_oz :: Unit
_oz = Unit "oz" _mass

_lb :: Unit
_lb = Unit "lb" _mass

_st :: Unit
_st = Unit "st" _mass

_cwt :: Unit
_cwt = Unit "cwt" _mass

_t :: Unit
_t = Unit "t" _mass

--
-- power units
--

_W :: Unit
_W = Unit "W" _power

_hp :: Unit
_hp = Unit "hp" _power

--
-- pressure units
--

_Pa :: Unit
_Pa = Unit "Pa" _pressure

_psi :: Unit
_psi = Unit "psi" _pressure

_bar :: Unit
_bar = Unit "bar" _pressure

--
-- speed units
--

_kph :: Unit
_kph = Unit "kph" _speed

_kn :: Unit
_kn = Unit "kn" _speed

_mph :: Unit
_mph = Unit "mph" _speed

--
-- storage units
--

_B :: Unit
_B = Unit "B" _storage

_b :: Unit
_b = Unit "b" _storage

--
-- temperature units
--

_Tc :: Unit
_Tc = Unit "Tc" _temperature

_Tf :: Unit
_Tf = Unit "Tf" _temperature

-- where
--   cToF x = x * (9 % 5) + 32
--   fToC x = (x - 32) * (5 % 9)

_Tk :: Unit
_Tk = Unit "Tk" _temperature

-- where
--   cToK x = x + 273.15
--   kToC x = x - 273.15

_Tr :: Unit
_Tr = Unit "Tr" _temperature

-- where
--   cToR x = x * (9 % 5) + 491.67
--   rToC x = (x - 491.67) * (5 % 9)

--
-- volume units
--

_L :: Unit
_L = Unit "L" _volume

_tsp :: Unit
_tsp = Unit "tsp" _volume

_tbsp :: Unit
_tbsp = Unit "tbsp" _volume

_floz :: Unit
_floz = Unit "floz" _volume

_c :: Unit
_c = Unit "c" _volume

_pt :: Unit
_pt = Unit "pt" _volume

_qt :: Unit
_qt = Unit "qt" _volume

_gal :: Unit
_gal = Unit "gal" _volume
