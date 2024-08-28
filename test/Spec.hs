{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

import Data.FileEmbed
import Test.Hspec
import Text.Parsec
import Tn.Builtins
import Tn.Conv
import Tn.Dims
import Tn.Expr
import Tn.Scope
import Tn.Script
import Tn.Units

main :: IO ()
main =
  let scope = defaultScope
   in hspec $ do
        testDims
        testConvs
        testUnits
        testScripts

testScript :: String
testScript = $(embedStringFile "scripts/tests/test.tn")

testDims :: SpecWith ()
testDims = do
  describe "Dims <> Dims" $ do
    it "mass <> length == mass length" $ do
      Dims [(_mass, 1)] <> Dims [(_length, 1)] == Dims [(_mass, 1), (_length, 1)]

  describe "Dims </> Dims" $ do
    it "mass^2 </> mass == mass" $ do
      Dims [(_mass, 2)] </> Dims [(_mass, 1)] == Dims [(_mass, 1)]

  describe "Fundamental Dims" $ do
    it "fundamentalDims _mass == [(_mass, 1)]" $ do
      fundamentalDims _mass == Dims [(_mass, 1)]
    it "fundamentalDims _speed == [(_length, 1), (_duration, -1)]" $ do
      fundamentalDims _speed == Dims [(_length, 1), (_duration, -1)]

testConvs :: SpecWith ()
testConvs = do
  describe "Conv <> Conv" $ do
    it "Base <> Base == Base" $ do
      isBase (Base <> Base) `shouldBe` True

-- it "Base <> Linear 1 == Linear 1" $ do
--   Base <> Linear 1 `shouldBe` Linear 1
-- it "Linear 1 <> Base == Linear 1" $ do
--   Linear 1 <> Base `shouldBe` Linear 1
-- it "Linear 2 <> Linear 3 == Linear 6" $ do
--   Linear 2 <> Linear 3 `shouldBe` Linear 6

testUnits :: SpecWith ()
testUnits = do
  describe "Units <> Units" $ do
    it "_g <> _m == _g _m" $ do
      Units [(_g, 1)] <> Units [(_m, 1)] == Units [(_g, 1), (_m, 1)]
    it "_m </> _s == _m/_s" $ do
      Units [(_m, 1)] </> Units [(_s, 1)] == Units [(_m, 1), (_s, -1)]

  describe "dims units" $ do
    it "dims [(_kph, 1)] == [(_length, 1), (_duration, -1)]" $ do
      dims (Units [(_kph, 1)]) == Dims [(_length, 1), (_duration, -1)]
    it "dims [(_kph, 2)] == [(_length, 2), (_duration, -2)]" $ do
      dims (Units [(_kph, 2)]) == Dims [(_length, 2), (_duration, -2)]

  describe "verifyUnits" $ do
    it "verifyUnits [(_m, 1), (_s, 1)] == True" $ do
      verifyUnits (Units [(_m, 1), (_s, 1)]) `shouldBe` True
    it "verifyUnits [(_s, 1), (_min, -1)] == False" $ do
      verifyUnits (Units [(_s, 1), (_min, -1)]) `shouldBe` False

  describe "Units ~= Units" $ do
    it "([(_s, 1)] ~= [(_min, 1)]) == True" $ do
      (Units [(_s, 1)] ~= Units [(_min, 1)]) `shouldBe` True
    it "([(_s, 1)] ~= [(_m, 1)]) == False" $ do
      (Units [(_s, 1)] ~= Units [(_m, 1)]) `shouldBe` False

  describe "mergeUnits" $ do
    it "mergeUnits [(_m, 1)] [(_ft, 1)] == [(_ft, 2)]" $ do
      mergeUnits (Units [(_m, 1)]) (Units [(_ft, 1)]) `shouldBe` Units [(_ft, 2)]

testScripts :: SpecWith ()
testScripts =
  let scope = loadScript "test.tn" testScript mempty
   in do
        describe "load script" $ do
          it "load test script" $ do
            isRight scope `shouldBe` True

          let Right s = scope
           in do
                it "check base dims" $ do
                  print s._dims
                  print s._units
                  length (baseDims s) `shouldBe` 2

{-
testDims = do
  describe "baseDims" $ do
    it "ft == Length" $ do
      dims "ft" == Dims [(Length, 1)]
    it "ft/s == Speed" $ do
      dims "ft/s" == baseDims Speed

testUnits script = do
  describe "parsing units" $ do
    it "ft == ft" $ do
      parseUnits script "ft" `shouldBe` Right "ft"
    it "ft/s == ft/s" $ do
      parseUnits script "ft/s" `shouldBe` Right "ft/s"
    it "asdf == invalid units" $ do
      parseUnits script "asdf" `shouldSatisfy` isLeft

  describe "combine units" $ do
    it "noUnits <> ft" $ do
      (noUnits <> "ft") `shouldBe` "ft"
    it "ft <> noUnits" $ do
      ("ft" <> noUnits) `shouldBe` "ft"
    it "ft <> ft" $ do
      ("ft" <> "ft") `shouldBe` ("ft^2" :: Units)
    it "ft <> s" $ do
      ("ft" <> "s") `shouldBe` ("ft s" :: Units)

  describe "map units" $ do
    it "recipUnits ft" $ do
      recipUnits "ft" `shouldBe` ("ft^-1" :: Units)
    it "recipUnits ft^2" $ do
      recipUnits "ft^2" `shouldBe` ("ft^-2" :: Units)
    it "recipUnits ft^-2" $ do
      recipUnits "ft^-2" `shouldBe` ("ft^2" :: Units)
    it "ft^2 </> ft" $ do
      "ft^2" </> "ft" `shouldBe` ("ft" :: Units)
    it "ft </> ft^2" $ do
      "ft" </> "ft^2" `shouldBe` ("ft^-1" :: Units)
    it "ft </> s" $ do
      "ft" </> "s" `shouldBe` ("ft/s" :: Units)

testScalars = do
  describe "scalars" $ do
    it "no units" $ do
      (1 :: Scalar) `shouldBe` Scalar 1 mempty mempty

  describe "scalar math operations" $ do
    testExpr "1 + 1" 2
    testExpr "1 - 1" 0
    testExpr "2 * 3" 6
    testExpr "6 / 3" 2
    testExpr "1 < 2" $ fromBool True
    testExpr "1 > 2" $ fromBool False
    testExpr "2 <= 2" $ fromBool True
    testExpr "2 <= 1" $ fromBool False
    testExpr "2 >= 2" $ fromBool True
    testExpr "2 >= 3" $ fromBool False
    testExpr "2 == 2" $ fromBool True
    testExpr "2 == 3" $ fromBool False
    testExpr "2 /= 2" $ fromBool False
    testExpr "2 /= 3" $ fromBool True
    testExpr "(2 ft)^2" "4 ft^2"
    testExpr "(3 ft^3)^2" "9 ft^6"
    testExpr "(4 ft^2)^0.5" "2 ft"
    testExpr "(4 ft)^0" 1
    testExpr "4 ft^0" 4
    testExpr "(4 ft)^-1" "0.25 ft^-1"

testConversions = do
  describe "basic conversions" $ do
    testExpr "1 ft : in" "12 in"
    testExpr "12 in : ft" "1 ft"
    testExpr "1 ft : cm" "30.48 cm"
    testExpr "1 ft^2 : cm^2" "929.03 cm^2"

  describe "si conversions" $ do
    testExpr "1 aL : L" "1e-18 L"
    testExpr "1 fL : L" "1e-15 L"
    testExpr "1 pL : L" "1e-12 L"
    testExpr "1 nL : L" "1e-9 L"
    testExpr "1 uL : L" "1e-6 L"
    testExpr "1 mL : L" "1e-3 L"
    testExpr "1 cL : L" "1e-2 L"
    testExpr "1 dL : L" "1e-1 L"
    testExpr "1 daL : L" "1e1 L"
    testExpr "1 hL : L" "1e2 L"
    testExpr "1 kL : L" "1e3 L"
    testExpr "1 ML : L" "1e6 L"
    testExpr "1 GL : L" "1e9 L"
    testExpr "1 TL : L" "1e12 L"
    testExpr "1 PL : L" "1e15 L"
    testExpr "1 EL : L" "1e18 L"

  describe "parent conversions" $ do
    testExpr "(10/2) ft" "5 ft"
    testExpr "(24 in/2) ft" "1 ft"
    testExpr "(10 J/2 s) W" "5 W"

  describe "multi-step conversions" $ do
    testExpr "1 cable : h" "2160 h"
    testExpr "1 gal : floz" "128 floz"

  describe "simplified conversions" $ do
    testExpr "1 ft^2 : in^2" "144 in^2"
    testExpr "1 m^3 : cm^3" "1000000 cm^3"

  describe "compound conversions" $ do
    testExpr "1 mi/hr : ft/s" "1.467 ft/s"

  describe "dimension conversions" $ do
    testExpr "1 ha : m^2" "10000 m^2"
    testExpr "2 N : kg m/s^2" "2 kg m/s^2"

  describe "complex dimension conversions" $ do
    testExpr "1 L : in^3" "61.02 in^3"
    testExpr "2 N : ft lb/min^2" "52077.69973 ft lb/min^2"

  describe "simple expressions" $ do
    testExpr "1 + 2" 3
    testExpr "1 - 2" (-1)
    testExpr "1 * 2" 2
    testExpr "1 / 2" 0.5
    testExpr "1 + 2 ft" "3 ft"
    testExpr "2 ft + 1" "3 ft"
    testExpr "1 ft + 1 in" "13 in"
    testExpr "12 in + 1 ft" "2 ft"

  describe "harmonized expressions" $ do
    testExpr "2 ft * 3" "6 ft"
    testExpr "2 * 3 ft" "6 ft"
    testExpr "1 ft * 2 in" "24 in^2"
    testExpr "12 in * 1 ft" "1 ft^2"
    testExpr "1 ft / 2 in" 6
    testExpr "1 ft * 2 in^-1" 24
    testExpr "1 ft * 2 in^2" "24 in^3"
    testExpr "1 m/s * 1 min" "60 m"

  describe "precision loss" $ do
    testExpr "((2 ft : in) : ft) : in" "24 in"

  describe "angle conversions" $ do
    testExpr "1 rad : deg" "57.3 deg"
    testExpr "1 rad : turn" "0.1592 turn"
    testExpr "1 rad : rev" "0.1592 rev"

  describe "area conversions" $ do
    testExpr "1 ha : m^2" "10000 m^2"
    testExpr "1 ha : acre" "2.471 acre"

  describe "duration conversions" $ do
    testExpr "1 s : min" "0.01667 min"
    testExpr "1 s : hr" "2.778e-4 hr"
    testExpr "1 s : day" "1.157e-5 day"

testArgs = do
  describe "single arguments" $ do
    testExprArgs [1] "_ ft" "1 ft"
    testExprArgs [1] "_ ft : in" "12 in"
    testExprArgs [2] "_ yd : ft" "6 ft"
    testExprArgs [1] "5 ft + _ in" "61 in"

  describe "multiple arguments" $ do
    testExprArgs [1, 2] "_ ft + _ in" "14 in"
    testExprArgs [2, 3, 1] "_ * _ - _" "5"

testFuncs script = do
  describe "built-in functions" $ do
    testScriptExpr script "[if true; 1; 2]" 1
    testScriptExpr script "[if false; 1; 2]" 2
    testScriptExpr script "[abs -8 ft]" "8 ft"
    testScriptExpr script "[signum -4 ft]" "-1 ft"
    testScriptExpr script "[sqrt 4 ft^2]" "2 ft"
    testScriptExpr script "[exp [log 16]]" 16
    testScriptExpr script "[truncate 4.4 in]" "4 in"
    testScriptExpr script "[floor 4.7 mi]" "4 mi"
    testScriptExpr script "[ceil 4.2 kn]" "5 kn"
    testScriptExpr script "[round 4.3 acre]" "4 acre"
    testScriptExpr script "[sin 90 deg]" 1
    testScriptExpr script "[cos [pi] : rad]" (-1)
    testScriptExpr script "[asin [sin 45 deg]] to deg" "45 deg"
    testScriptExpr script "[asinh [sinh 1.4 rad]]" "1.4 rad"

testScript script = do
  -- describe "script units" $ do
  --  testScriptExpr (useSystem script "astro") "1 AU : mi" "92955807.3026 mi"

  describe "script functions" $ do
    testScriptExpr script "[areaOfCircle 4 m]" "50.266 m^2"
    testScriptExpr script "[areaOfRect 2 ft; 3 in]" "72 in^2"
    testScriptExpr script "[volumeOfTorus 2 in; 12 in]" "947.482 in^3"
    testScriptExpr script "[transferRate 10 GB; 1 hr]" "2.844 MB/s"
-}
