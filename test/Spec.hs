{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

import Data.FileEmbed
import Test.Hspec
import Text.Parsec
import Tn.Builtins
import Tn.Conv
import Tn.Units

main :: IO ()
main =
  hspec $ do
    testUnits
    testConvs

-- testScript :: String
-- testScript = $(embedStringFile "scripts/tests/test.tn")

testUnits :: SpecWith ()
  describe "Unit equality" $ do
    it "_m == _m" $ do
      _m `shouldBe` _m
    it "_g /= _m" $ do
      _g `shouldNotBe` _m

  describe "Base dimensions" $ do
    it "_J == [(_mass, 1), (_length, 2), (_time, -2)]" $ do
      baseDims [(_J, 1)] `shouldBe` Dims [(_mass, 1), (_length, 2), (_time, -2)]
    it "_L^2 == [(_length, 6)]" $ do
      baseDims [(_L, 2)] `shouldBe` Dims [(_length, 6)]

  describe "Base units" $ do
    it "_J == [(_kg, 1), (_m, 2), (_s, -2)]" $ do
      baseUnits [(_J, 1)] `shouldBe` Dims [(_kg, 1), (_m, 2), (_s, -2)]
    it "_L^2 == [(_m, 6)]" $ do
      baseUnits [(_L, 2)] `shouldBe` Dims [(_m, 6)]

  describe "Base conversions" $ do
    it "_ft -> _m" $ do
      unitsToConv [(_ft, 1)] [(_m, 2)] `shouldBe` [(_ft, _m)]

testConvs :: SpecWith ()
testConvs = do
  let gr = siConvs _m

      -- test units
      _cm = Unit "cm" _length
      _km = Unit "km" _length

  describe "Simple conversions" $ do
    it "_m to _cm == 100" $ do
      fmap (applyConv 1) (findConv _m _cm gr) `shouldBe` Just 100
    it "_km to _m == 1000" $ do
      fmap (applyConv 1) (findConv _km _m gr) `shouldBe` Just 1000

  describe "Path conversions" $ do
    it "_km to _cm == 100,000" $ do
      fmap (applyConv 1) (findConv _km _cm gr) `shouldBe` Just 100_000


