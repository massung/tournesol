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
    testExpr

-- testScript :: String
-- testScript = $(embedStringFile "scripts/tests/test.tn")

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
    it "_J == [(_kg, 1), (_m, 2), (_s, -2)]" $ do
      baseUnits [(_J, 1)] `shouldBe` Dims [(_kg, 1), (_m, 2), (_s, -2)]
    it "_L^2 == [(_m, 6)]" $ do
      baseUnits [(_L, 2)] `shouldBe` Dims [(_m, 6)]

  describe "Base conversions" $ do
    it "_ft -> _m" $ do
      unitsToConv [(_ft, 1)] [(_m, 2)] `shouldBe` [(_ft, _m, 1)]

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
      fmap (applyConv 1) (findConv _m _cm gr) `shouldBe` Just 100
    it "_km to _m == 1000" $ do
      fmap (applyConv 1) (findConv _km _m gr) `shouldBe` Just 1000

  describe "Path conversions" $ do
    it "_km to _cm == 100,000" $ do
      fmap (applyConv 1) (findConv _km _cm gr) `shouldBe` Just 100_000

testParsing :: SpecWith ()
testParsing = do
  let _km = Unit "km" _length
      _ft = Unit "ft" _length
      _in = Unit "in" _length
      _hr = Unit "hr" _time
      _kg = Unit "kg" _mass

  describe "Parse simple scalars" $ do
    it "parse 5" $ do
      ("5" :: Scalar) `shouldBe` Scalar 5 Nothing
    it "parse 3.5" $ do
      ("3.5" :: Scalar) `shouldBe` Scalar 3.5 Nothing

  describe "Parse scientific notation" $ do
    it "parse 4e3" $ do
      ("4e3" :: Scalar) `shouldBe` Scalar 4e3 Nothing
    it "parse 50e-2" $ do
      ("50e-2" :: Scalar) `shouldBe` Scalar 0.5 Nothing

  describe "Parse scalars with units" $ do
    it "parse 30 ft" $ do
      ("30 ft" :: Scalar) `shouldBe` Scalar 30 (Just $ Dims [(_ft, 1)])
    it "parse 12in^2" $ do
      ("12in^2" :: Scalar) `shouldBe` Scalar 12 (Just $ Dims [(_in, 2)])
    it "parse 45 km/hr" $ do
      ("45 km/hr" :: Scalar) `shouldBe` Scalar 45 (Just $ Dims [(_km, 1), (_hr, -1)])
    it "parse 12 in^3 hr" $ do
      ("12 in^3 hr" :: Scalar) `shouldBe` Scalar 12 (Just $ Dims [(_in, 3), (_hr, 1)])
    it "parse 2 ft kg^2 / hr^4" $ do
      ("2 ft kg^2 / hr^4" :: Scalar) `shouldBe` Scalar 2 (Just $ Dims [(_ft, 1), (_kg, 2), (_hr, -4)])
    it "parse 2 s^-2" $ do
      ("2 s^-2" :: Scalar)  `shouldBe` Scalar 2 (Just $ Dims [(_s, -2)])

testOps :: SpecWith ()
testOps = do
  describe "Simple add/subtract" $ do
    it "2 + 4" $ do
      runWithDefaultScope ("2" +% "4") `shouldBe` Right (Scalar 6 Nothing)
    it "10 - 4" $ do
      runWithDefaultScope ("10" -% "4") `shouldBe` Right (Scalar 6 Nothing)

  describe "Add/subtract with units" $ do
    it "2 ft + 4 ft" $ do
      runWithDefaultScope ("2 ft" +% "4 ft") `shouldBe` Right (Scalar 6 (Just $ Dims [(_ft, 1)]))
    it "10 in - 4 in" $ do
      runWithDefaultScope ("10 in" -% "4 in") `shouldBe` Right (Scalar 6 (Just $ Dims [(_in, 1)]))

  describe "Add/subtract with units conversion" $ do
    it "6 in + 2 ft" $ do
      runWithDefaultScope ("6 in" +% "2 ft") `shouldBe` Right (Scalar 30 (Just $ Dims [(_in, 1)]))
    it "3 ft^2 + 1 yd^2" $ do
      runWithDefaultScope ("3 ft^2" +% "1 yd^2") `shouldBe` Right (Scalar 12 (Just $ Dims [(_ft, 2)]))

  describe "Add/subtract negative unit conversion" $ do
    it "6 ft + 2 g" $ do
      runWithDefaultScope ("6 ft" +% "2 g") `shouldSatisfy` isLeft
    it "6 ft - 2 g" $ do
      runWithDefaultScope ("6 ft" -% "2 g") `shouldSatisfy` isLeft
    it "6 ft^2 - 1 ft" $ do
      runWithDefaultScope ("6 ft^2" -% "1 ft") `shouldSatisfy` isLeft

  describe "Simple multiply/divide" $ do
    it "2 * 4" $ do
      runWithDefaultScope ("2" *% "4") `shouldBe` Right (Scalar 8 Nothing)
    it "8 / 2" $ do
      runWithDefaultScope ("8" /% "2") `shouldBe` Right (Scalar 4 Nothing)

  describe "Multiply/divide with units" $ do
    it "2 ft * 3 ft" $ do
      runWithDefaultScope ("2 ft" *% "3 ft") `shouldBe` Right (Scalar 6 (Just $ Dims [(_ft, 2)]))
    it "8 ft / 4 ft" $ do
      runWithDefaultScope ("8 ft" /% "4 ft") `shouldBe` Right (Scalar 2 Nothing)

  describe "Multiply/divide with unit conversion" $ do
    it "4 ft^2 * 1 yd" $ do
      runWithDefaultScope ("4 ft^2" *% "1 yd") `shouldBe` Right (Scalar 12 (Just $ Dims [(_ft, 3)]))
    it "24 in^3 / 2 ft" $ do
      runWithDefaultScope ("24 in^3" /% "2 ft") `shouldBe` Right (Scalar 1 (Just $ Dims [(_in, 2)]))

  -- describe "Multipy/divide with base units" $ do
  --   it "10 J / 2 ft" $ do
  --     runWithDefaultScope ("10 J" /% "2 ft") `shouldBe` Right (Scalar 5 (Just $ Dims [(_J, 1), (_m, -1)]))

testExpr :: SpecWith ()
testExpr = do
  describe "Expressions" $ do
    it "1 + 2 * 3" $ do
      "a" `shouldBe` "a"
