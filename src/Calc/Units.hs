{-# LANGUAGE OverloadedStrings #-}

module Calc.Units where

import Calc.Conv
import Calc.Dims
import Control.Applicative
import Data.Foldable as F
import Data.List as L
import Data.List.Extra
import Data.Map.Strict as M
import Data.Maybe
import Data.Ratio
import Data.String

data Unit = Unit {dim :: !Dim, symbol :: !String, conv :: !Conv}

instance Eq Unit where
  (==) a b = symbol a == symbol b

instance Ord Unit where
  compare a b = compare (symbol a) (symbol b)

instance IsString Unit where
  fromString s = fromMaybe (error $ "no unit " ++ show s) $ M.lookup s unitMap

instance Show Unit where
  show = symbol

newtype Units = Units (Map Unit Rational)
  deriving (Eq, Ord)

instance Semigroup Units where
  (<>) (Units a) (Units b) = Units $ M.filter (/= 0) $ M.unionWith (+) a b

instance Monoid Units where
  mempty = Units mempty

instance Show Units where
  show (Units u)
    | F.null num = showExps' den
    | F.null den = showExps' num
    | otherwise = showExps' num ++ "/" ++ showExps' (M.map abs den)
    where
      (num, den) = M.partition (> 0) u

      -- display a single unit with exponent
      showExp (x, 1) = show x
      showExp (x, n) =
        if denominator n == 1
          then show x ++ "^" ++ show (numerator n)
          else show x ++ "^" ++ show (fromRational n)

      -- concatenate units together
      showExps' = unwords . L.map showExp . M.toList

_pi = toRational pi

--
-- angle units
--

_radian = Unit {dim = Angle, symbol = "rad", conv = Base}

_gradian = Unit {dim = Angle, symbol = "grad", conv = Linear (200 / _pi)}

_degree = Unit {dim = Angle, symbol = "deg", conv = Linear (180 / _pi)}

_rev = Unit {dim = Angle, symbol = "rev", conv = Linear ((1 % 2) / _pi)}

_turn = Unit {dim = Angle, symbol = "turn", conv = Linear ((1 % 2) / _pi)}

--
-- area units
--

_hectare = Unit {dim = Area, symbol = "ha", conv = Linear 1e-4}

_acre = Unit {dim = Area, symbol = "acre", conv = Linear (78125 % 316160658)}

--
-- duration units
--

_second = Unit {dim = Duration, symbol = "s", conv = Base}

_minute = Unit {dim = Duration, symbol = "min", conv = Linear (1 % 60)}

_hour = Unit {dim = Duration, symbol = "hr", conv = Linear (1 % 3600)}

_day = Unit {dim = Duration, symbol = "day", conv = Linear (1 % 86400)}

--
-- electrical units
--

_farad = Unit {dim = Capacitance, symbol = "F", conv = Base}

_coulomb = Unit {dim = Charge, symbol = "C", conv = Base}

_ampere = Unit {dim = Current, symbol = "A", conv = Base}

_ohm = Unit {dim = Resistance, symbol = "O", conv = Base}

_volt = Unit {dim = Voltage, symbol = "V", conv = Base}

--
-- energy units
--

_joule = Unit {dim = Energy, symbol = "J", conv = Base}

_btu = Unit {dim = Energy, symbol = "BTU", conv = Linear (50000000 % 52752792631)}

_therm = Unit {dim = Energy, symbol = "thm", conv = Linear (1 % 105480400)}

_electronVolt = Unit {dim = Energy, symbol = "eV", conv = Linear (5000000000000000000000000000 % 801088317)}

--
-- force units
--

_newton = Unit {dim = Force, symbol = "N", conv = Base}

_poundForce = Unit {dim = Force, symbol = "lbf", conv = Linear (2000000000000 % 8896443230521)}

_pond = Unit {dim = Force, symbol = "pond", conv = Linear (20000000 % 1961333)}

--
-- frequency units
--

_hertz = Unit {dim = Frequency, symbol = "hz", conv = Base}

--
-- length units
--

_meter = Unit {dim = Length, symbol = "m", conv = Base}

_mil = Unit {dim = Length, symbol = "mil", conv = Linear 39370}

_inch = Unit {dim = Length, symbol = "in", conv = Linear (5000 % 127)}

_hand = Unit {dim = Length, symbol = "h", conv = Linear (3750 % 381)}

_foot = Unit {dim = Length, symbol = "ft", conv = Linear (1250 % 381)}

_yard = Unit {dim = Length, symbol = "yd", conv = Linear (1250 % 1143)}

_fathom = Unit {dim = Length, symbol = "ftm", conv = Linear (625 % 1143)}

_chain = Unit {dim = Length, symbol = "ch", conv = Linear (625 % 12573)}

_furlong = Unit {dim = Length, symbol = "fur", conv = Linear (125 % 25146)}

_mile = Unit {dim = Length, symbol = "mi", conv = Linear (125 % 201168)}

_league = Unit {dim = Length, symbol = "lea", conv = Linear (125 % 603504)}

_cable = Unit {dim = Length, symbol = "cable", conv = Linear (125 % 27432)}

_nauticalMile = Unit {dim = Length, symbol = "nmi", conv = Linear (1 % 1852)}

_link = Unit {dim = Length, symbol = "link", conv = Linear (62500 % 12573)}

_rod = Unit {dim = Length, symbol = "rod", conv = Linear (2500 % 12573)}

--
-- astronomical length units
--

_parsec = Unit {dim = Length, symbol = "pc", conv = Linear (1 % 30856775814913670)}

_astronomicalUnit = Unit {dim = Length, symbol = "au", conv = Linear (1 % 149597870700)}

_lightYear = Unit {dim = Length, symbol = "ly", conv = Linear (1 % 9460730472580800)}

--
-- mass units
--

_gram = Unit {dim = Mass, symbol = "g", conv = Linear 1000}

_ounce = Unit {dim = Mass, symbol = "oz", conv = Linear (1600000000 % 45359237)}

_pound = Unit {dim = Mass, symbol = "lb", conv = Linear (100000000 % 45359237)}

_stone = Unit {dim = Mass, symbol = "st", conv = Linear (50000000 % 317514659)}

_hundredweight = Unit {dim = Mass, symbol = "cwt", conv = Linear (1000000 % 45359237)}

_ton = Unit {dim = Mass, symbol = "t", conv = Linear (50000 % 45359237)}

--
-- power units
--

_watt = Unit {dim = Power, symbol = "W", conv = Base}

_horsepower = Unit {dim = Power, symbol = "hp", conv = Linear 0.001341}

--
-- pressure units
--

_pascal = Unit {dim = Pressure, symbol = "Pa", conv = Base}

_psi = Unit {dim = Pressure, symbol = "psi", conv = Linear 145e-6}

_bar = Unit {dim = Pressure, symbol = "bar", conv = Linear 1e-5}

--
-- speed units
--

_kph = Unit {dim = Speed, symbol = "kph", conv = Linear (18 % 5)}

_knot = Unit {dim = Speed, symbol = "kn", conv = Linear (3125 % 1397)}

_mph = Unit {dim = Speed, symbol = "mph", conv = Linear (900 % 463)}

--
-- storage units
--

_bit = Unit {dim = Storage, symbol = "b", conv = Linear 8}

_byte = Unit {dim = Storage, symbol = "B", conv = Base}

--
-- temperature units
--

_celcius = Unit {dim = Temperature, symbol = "Tc", conv = Base}

_fahrenheit = Unit {dim = Temperature, symbol = "Tf", conv = Function cToF fToC}
  where
    cToF x = x * (9 % 5) + 32
    fToC x = (x - 32) * (5 % 9)

_kelvin = Unit {dim = Temperature, symbol = "Tk", conv = Function cToK kToC}
  where
    cToK x = x + 273.15
    kToC x = x - 273.15

_rankine = Unit {dim = Temperature, symbol = "Tr", conv = Function cToR rToC}
  where
    cToR x = x * (9 % 5) + 491.67
    rToC x = (x - 491.67) * (5 % 9)

--
-- volume units
--

_liter = Unit {dim = Volume, symbol = "L", conv = Linear 1000}

_teaspoon = Unit {dim = Volume, symbol = "tsp", conv = Linear (96000000000000 % 473176473)}

_tablespoon = Unit {dim = Volume, symbol = "tbsp", conv = Linear (32000000000000 % 473176473)}

_fluidOunce = Unit {dim = Volume, symbol = "floz", conv = Linear (16000000000000 % 473176473)}

_cup = Unit {dim = Volume, symbol = "c", conv = Linear (2000000000000 % 473176473)}

_pint = Unit {dim = Volume, symbol = "pt", conv = Linear (1000000000000 % 473176473)}

_quart = Unit {dim = Volume, symbol = "qt", conv = Linear (500000000000 % 473176473)}

_gallon = Unit {dim = Volume, symbol = "gal", conv = Linear (125000000000 % 473176473)}

siUnits u = [siUnit conversion | conversion <- siConversions]
  where
    siUnit (p, c) = u {symbol = p ++ symbol u, conv = conv u <> c}

storageUnits u = [storageUnit conversion | conversion <- storageConversions]
  where
    storageUnit (p, c) = u {symbol = p ++ symbol u, conv = conv u <> c}

unitMap :: Map String Unit
unitMap = F.foldl' (\m u -> M.insert (symbol u) u m) mempty units
  where
    units =
      concat
        [ [ _acre,
            _ampere,
            _astronomicalUnit,
            _bar,
            _bit,
            _btu,
            _byte,
            _cable,
            _celcius,
            _chain,
            _coulomb,
            _cup,
            _day,
            _degree,
            _electronVolt,
            _fahrenheit,
            _farad,
            _fathom,
            _fluidOunce,
            _foot,
            _furlong,
            _gallon,
            _gradian,
            _gram,
            _hand,
            _hectare,
            _horsepower,
            _hour,
            _hundredweight,
            _hertz,
            _inch,
            _joule,
            _kelvin,
            _knot,
            _kph,
            _league,
            _lightYear,
            _link,
            _liter,
            _meter,
            _mil,
            _mile,
            _minute,
            _mph,
            _nauticalMile,
            _newton,
            _ohm,
            _ounce,
            _parsec,
            _pascal,
            _pint,
            _pond,
            _pound,
            _poundForce,
            _psi,
            _quart,
            _radian,
            _rankine,
            _rev,
            _rod,
            _second,
            _stone,
            _tablespoon,
            _teaspoon,
            _therm,
            _ton,
            _turn,
            _volt,
            _watt,
            _yard
          ],
          -- si units
          siUnits _ampere,
          siUnits _coulomb,
          siUnits _electronVolt,
          siUnits _farad,
          siUnits _gram,
          siUnits _hertz,
          siUnits _joule,
          siUnits _liter,
          siUnits _meter,
          siUnits _newton,
          siUnits _ohm,
          siUnits _pascal,
          siUnits _second,
          siUnits _volt,
          siUnits _watt,
          -- storage units
          storageUnits _bit,
          storageUnits _byte
        ]

nullUnits (Units u) = M.null u

dims (Units u) = mconcat [powDims n $ baseDims (dim unit) | (unit, n) <- M.toList u]

mapUnits f (Units u) = Units $ M.filter (/= 0) $ M.map f u

recipUnits = mapUnits negate

(</>) a b = a <> recipUnits b

unitsConv (Units u) = F.foldl' (<>) Base [powConv n conv | (Unit {conv=conv}, n) <- M.toList u]

unitsConvScale from to = recipConv (unitsConv from) <> unitsConv to

validateUnits (Units u) = all (== 1) $ M.foldlWithKey' countDims M.empty u
  where
    countDims m u _ = M.alter (\x -> (+ 1) <$> x <|> Just 1) (dim u) m

harmonizeUnits from@(Units a) to@(Units b) = Units $ M.mapKeys mapUnit a
  where
    mapUnit u = headDef u [k | (k, _) <- M.toList b, dim u == dim k]
