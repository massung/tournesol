module Calc.Parser.Dims where

import Calc.Dims
import Calc.Parser.Lexer
import Data.Foldable as F
import Text.Parsec
import Text.Parsec.Token

dimParser :: Parsec String st (Dim, Rational)
dimParser = do
  s <- identifier lexer >>= fromString
  e <- exponentParser
  return (s, e)
  where
    fromString s
      | s == "angle" = return Angle
      | s == "area" = return Area
      | s == "capacitance" = return Capacitance
      | s == "charge" = return Charge
      | s == "current" = return Current
      | s == "duration" = return Duration
      | s == "energy" = return Energy
      | s == "force" = return Force
      | s == "frequency" = return Frequency
      | s == "length" = return Length
      | s == "mass" = return Mass
      | s == "power" = return Power
      | s == "pressure" = return Pressure
      | s == "resistance" = return Resistance
      | s == "speed" = return Speed
      | s == "storage" = return Storage
      | s == "temperature" = return Temperature
      | s == "voltage" = return Voltage
      | s == "volume" = return Volume
      | otherwise = fail $ "no dimension " ++ s

dimsParser :: Parsec String st Dims
dimsParser = do
  dims <- brackets lexer $ sepBy dimParser (lexeme lexer $ char ';')
  return $ F.foldl' (<>) mempty [powDims e $ baseDims dim | (dim, e) <- dims]
