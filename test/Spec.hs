{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

import Data.FileEmbed
import Test.Hspec
import Text.Parsec
import Tn.Debug

main :: IO ()
main =
  hspec $ do
    testUnits
    testConvs
    testParsing
    testOps
    testExprs

    -- load the test script before this spec action
    case loadScript "test.tn" embeddedScript mempty of
      Left err -> error $ show err
      Right scope -> testScript scope

embeddedScript :: String
embeddedScript = $(embedStringFile "scripts/test.tn")

testScope :: Scope
testScope =
  defaultScope
    { _epsilon = 5e-3
    }

runWithTestScope :: OpResultT Scalar -> Either String Scalar
runWithTestScope = runWithScope testScope

evalWithTestScope :: String -> Either String Scalar
evalWithTestScope = evalWithScope testScope

testUnits :: SpecWith ()
testUnits = do
  describe "Unit equality" $ do
    it "_m == _m" $ do
      _m `shouldBe` _m
    it "_g /= _m" $ do
      _g `shouldNotBe` _m

  describe "Base dimensions" $ do
    it "_J == [(_mass, 1), (_length, 2), (_time, -2)]" $ do
      baseDims [(_J, 1)] `shouldBe` Dims [("mass", 1), ("length", 2), ("time", -2)]
    it "_L^2 == [(_length, 6)]" $ do
      baseDims [(_L, 2)] `shouldBe` Dims [("length", 6)]

  describe "Base units" $ do
    let _cm = Unit "cm" _length

    it "_J == [(_kg, 1), (_m, 2), (_s, -2)]" $ do
      baseUnits [(_J, 1)] `shouldBe` Dims [(_kg, 1), (_m, 2), (_s, -2)]
    it "_L^2 == [(_cm, 6)]" $ do
      baseUnits [(_L, 2)] `shouldBe` Dims [(_cm, 6)]

  describe "Base conversions" $ do
    it "_ft -> _m" $ do
      unitsToConv [(_ft, 1)] [(_m, 2)] `shouldBe` [(_ft, _m, 1)]

  describe "Validating units" $ do
    it "_ft _g" $ do
      verifyUnits [(_ft, 1), (_g, 1)] `shouldBe` True
    it "_ft _m" $ do
      verifyUnits [(_ft, 1), (_m, 1)] `shouldBe` False
    it "_ft^2" $ do
      verifyUnits [(_ft, 2)] `shouldBe` True
    it "_ft^2 _ft" $ do
      verifyUnits [(_ft, 2), (_ft, 1)] `shouldBe` True

testConvs :: SpecWith ()
testConvs = do
  let gr = defaultScope._convs

      -- some units
      _km = Unit "km" _length
      _cm = Unit "cm" _length
      _ft = Unit "ft" _length
      _in = Unit "in" _length

  describe "Simple conversions" $ do
    it "_m to _cm == 100" $ do
      fmap (applyConv 1) (findConv (_m, 1) _cm gr) `shouldBe` Just 100
    it "_km to _m == 1000" $ do
      fmap (applyConv 1) (findConv (_km, 1) _m gr) `shouldBe` Just 1000

  describe "Path conversions" $ do
    it "_km to _cm == 100,000" $ do
      fmap (applyConv 1) (findConv (_km, 1) _cm gr) `shouldBe` Just 100_000

testParsing :: SpecWith ()
testParsing = do
  let _km = Unit "km" _length
      _ft = Unit "ft" _length
      _in = Unit "in" _length
      _hr = Unit "hr" _time
      _kg = Unit "kg" _mass

  describe "Parse units" $ do
    it "kg" $ do
      ("kg" :: Units) `shouldBe` [(_kg, 1)]
    it "kg km^2" $ do
      ("kg km^2" :: Units) `shouldBe` [(_kg, 1), (_km, 2)]
    it "kg km^2 / hr^2" $ do
      ("kg km^2 / hr^2" :: Units) `shouldBe` [(_kg, 1), (_km, 2), (_hr, -2)]

  describe "Parse simple scalars" $ do
    it "5" $ do
      ("5" :: Scalar) `shouldBe` Scalar 5 Nothing
    it "3.5" $ do
      ("3.5" :: Scalar) `shouldBe` Scalar 3.5 Nothing

  describe "Parse scientific notation" $ do
    it "4e3" $ do
      ("4e3" :: Scalar) `shouldBe` Scalar 4e3 Nothing
    it "50e-2" $ do
      ("50e-2" :: Scalar) `shouldBe` Scalar 0.5 Nothing

  describe "Parse scalars with units" $ do
    it "30 ft" $ do
      ("30 ft" :: Scalar) `shouldBe` Scalar 30 (Just $ Dims [(_ft, 1)])
    it "12in^2" $ do
      ("12in^2" :: Scalar) `shouldBe` Scalar 12 (Just $ Dims [(_in, 2)])
    it "45 km/hr" $ do
      ("45 km/hr" :: Scalar) `shouldBe` Scalar 45 (Just $ Dims [(_km, 1), (_hr, -1)])
    it "12 in^3 hr" $ do
      ("12 in^3 hr" :: Scalar) `shouldBe` Scalar 12 (Just $ Dims [(_in, 3), (_hr, 1)])
    it "2 ft kg^2 / hr^4" $ do
      ("2 ft kg^2 / hr^4" :: Scalar) `shouldBe` Scalar 2 (Just $ Dims [(_ft, 1), (_kg, 2), (_hr, -4)])
    it "2 s^-2" $ do
      ("2 s^-2" :: Scalar) `shouldBe` Scalar 2 (Just $ Dims [(_s, -2)])

testOps :: SpecWith ()
testOps = do
  describe "Simple add/subtract" $ do
    it "2 + 4" $ do
      runWithTestScope ("2" +% "4") `shouldBe` Right 6
    it "10 - 4" $ do
      runWithTestScope ("10" -% "4") `shouldBe` Right 6

  describe "Add/subtract with units" $ do
    it "2 ft + 4 ft" $ do
      runWithTestScope ("2 ft" +% "4 ft") `shouldBe` Right "6 ft"
    it "10 in - 4 in" $ do
      runWithTestScope ("10 in" -% "4 in") `shouldBe` Right "6 in"

  describe "Add/subtract with units conversion" $ do
    it "6 in + 2 ft" $ do
      runWithTestScope ("6 in" +% "2 ft") `shouldBe` Right "30 in"
    it "3 ft^2 + 1 yd^2" $ do
      runWithTestScope ("3 ft^2" +% "1 yd^2") `shouldBe` Right "12 ft^2"

  describe "Add/subtract negative unit conversion" $ do
    it "6 ft + 2 g" $ do
      runWithDefaultScope ("6 ft" +% "2 g") `shouldSatisfy` isLeft
    it "6 ft - 2 g" $ do
      runWithTestScope ("6 ft" -% "2 g") `shouldSatisfy` isLeft
    it "6 ft^2 - 1 ft" $ do
      runWithTestScope ("6 ft^2" -% "1 ft") `shouldSatisfy` isLeft

  describe "Simple multiply/divide" $ do
    it "2 * 4" $ do
      runWithTestScope ("2" *% "4") `shouldBe` Right 8
    it "8 / 2" $ do
      runWithTestScope ("8" /% "2") `shouldBe` Right 4

  describe "Multiply/divide with units" $ do
    it "2 ft * 3 ft" $ do
      runWithTestScope ("2 ft" *% "3 ft") `shouldBe` Right "6 ft^2"
    it "8 ft / 4 ft" $ do
      runWithTestScope ("8 ft" /% "4 ft") `shouldBe` Right 2

  describe "Multiply/divide with unit conversion" $ do
    it "4 ft^2 * 1 yd" $ do
      runWithTestScope ("4 ft^2" *% "1 yd") `shouldBe` Right "12 ft^3"
    it "24 in^3 / 2 ft" $ do
      runWithTestScope ("24 in^3" /% "2 ft") `shouldBe` Right "1 in^2"

  describe "Multipy/divide with base units" $ do
    it "10 J / 2 ft" $ do
      runWithTestScope ("10 J" /% "2 ft") `shouldBe` Right "16.4042 J/m"

testExprs :: SpecWith ()
testExprs = do
  describe "Simple expressions" $ do
    it "2 + 3 - 1" $ do
      evalWithTestScope "2 + 3 - 1" `shouldBe` Right 4
    it "1 * 3 + 2" $ do
      evalWithTestScope "1 * 3 + 2" `shouldBe` Right 5
    it "1 + 2 * 3" $ do
      evalWithTestScope "1 + 2 * 3" `shouldBe` Right 7
    it "(1 + 2) * 3" $ do
      evalWithTestScope "(1 + 2) * 3" `shouldBe` Right 9

  describe "Simple expressions with units" $ do
    it "1 ft + 2" $ do
      evalWithTestScope "1 ft + 2" `shouldBe` Right "3 ft"
    it "1 + 2 ft" $ do
      evalWithTestScope "1 + 2 ft" `shouldBe` Right "3 ft"
    it "3m * 2m" $ do
      evalWithTestScope "3m * 2m" `shouldBe` Right "6m^2"

  describe "Expressions with unit conversions" $ do
    it "1 ft + 1 yd" $ do
      evalWithTestScope "1 ft + 1 yd" `shouldBe` Right "4 ft"
    it "1 yd + 3 ft" $ do
      evalWithTestScope "1 yd + 3 ft" `shouldBe` Right "2 yd"

testScript :: Scope -> SpecWith ()
testScript scope = do
  describe "Scripts" $ do
    context "Test unit conversions" $ do
      it "1 yd == 3 ft" $ do
        evalWithScope scope "1 yd == 3 ft" `shouldBe` Right 1
      it "1 mi == 1760 yd" $ do
        evalWithScope scope "1 mi == 1760 yd" `shouldBe` Right 1
      it "1 ft == 12 in" $ do
        evalWithScope scope "1 ft == 12 in" `shouldBe` Right 1
      it "1 km == 1000 m" $ do
        evalWithScope scope "1 km == 1000 m" `shouldBe` Right 1
      it "1 day == 86400 s" $ do
        evalWithScope scope "1 day == 86400 s" `shouldBe` Right 1

    context "Test derived units" $ do
      it "1 mph == 42240 yd/day" $ do
        evalWithScope scope "1 mph == 42240 yd/day" `shouldBe` Right 1
      it "2 kph == 200000 cm/hr" $ do
        evalWithScope scope "2 kph == 200000 cm/hr" `shouldBe` Right 1

    context "Test unit conversion between derived units" $ do
      it "10 kph : mph" $ do
        evalWithScope scope "10 kph : mph" `shouldSatisfy` isRight

    context "Test compound units" $ do
      it "1 acre : yd^2" $ do
        evalWithScope scope "1 acre : yd^2" `shouldBe` Right "4840 yd^2"
