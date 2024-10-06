{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

import Data.FileEmbed
import Test.Hspec
import Tn.Debug
import Prelude hiding (Arg)

main :: IO ()
main =
  hspec $ do
    testUnits
    testConversions
    testScalars
    testScript

embeddedScript :: String
embeddedScript = $(embedStringFile "test/test.tn")

testScope :: Scope
testScope = case loadScript "test" embeddedScript mempty of
  Left e -> error $ show (SyntaxError e)
  Right scope -> scope

testExpr :: String -> Scalar -> SpecWith (Arg Expectation)
testExpr expr res =
  it expr $ do
    evalWithDefaultScope expr `shouldBe` Right res

testWithScope :: String -> Scalar -> SpecWith (Arg Expectation)
testWithScope expr res =
  it expr $ do
    evalWithScope expr testScope `shouldBe` Right res

testUnits :: SpecWith ()
testUnits = do
  describe "Unit equality" $ do
    it "m == m" $ do
      "m" `shouldBe` ("m" :: Unit)
    it "g /= m" $ do
      "g" `shouldNotBe` ("m" :: Unit)

  describe "Base dimensions" $ do
    it "ft -> [length]" $ do
      baseDims "ft" `shouldBe` [("[length]", 1)]
    it "J -> [mass][length]^2/[time]^2" $ do
      baseDims "J" `shouldBe` [("[mass]", 1), ("[length]", 2), ("[time]", -2)]
    it "L^2 -> [length]^6" $ do
      baseDims "L^2" `shouldBe` [("[length]", 6)]

  describe "Base units" $ do
    it "m -> m" $ do
      baseUnits "m" `shouldBe` (1, "m")
    it "J -> kg m^2/s^2" $ do
      baseUnits "J" `shouldBe` (1, "kg m^2/s^2")
    it "lbf -> slug ft/s" $ do
      baseUnits "lbf" `shouldBe` (1, "slug ft/s^2")
    it "mph -> mi/hr" $ do
      baseUnits "mph" `shouldBe` (1, "mi/hr")

  describe "Map dimensions to units" $ do
    it "m -> ([length], (m, 1))" $ do
      mapBaseUnitDims "m" `shouldBe` [("[length]", ("m", 1))]

testConversions :: SpecWith ()
testConversions = do
  describe "Simple conversions" $ do
    testExpr "1 ft : in" "12 in"
    testExpr "12 in : ft" "1 ft"

  describe "SI conversions" $ do
    testExpr "(1 aL : L)" "1 % 1000000000000000000 L"
    testExpr "(1 fL : L)" "1 % 1000000000000000 L"
    testExpr "(1 pL : L)" "1 % 1000000000000 L"
    testExpr "(1 nL : L)" "1 % 1000000000 L"
    testExpr "(1 uL : L)" "1 % 1000000 L"
    testExpr "(1 mL : L)" "1 % 1000 L"
    testExpr "(1 cL : L)" "1 % 100 L"
    testExpr "(1 dL : L)" "1 % 10 L"
    testExpr "(1 daL : L)" "10 L"
    testExpr "(1 hL : L)" "100 L"
    testExpr "(1 kL : L)" "1000 L"
    testExpr "(1 ML : L)" "1000000 L"
    testExpr "(1 GL : L)" "1000000000 L"
    testExpr "(1 TL : L)" "1000000000000 L"
    testExpr "(1 PL : L)" "1000000000000000 L"
    testExpr "(1 EL : L)" "1000000000000000000 L"

  describe "Exponent conversions" $ do
    testExpr "9 ft^2 : yd^2" "1 yd^2"
    testExpr "1 ft^2 : in^2" "144 in^2"
    testExpr "1 yd^2 : in^2" "1296 in^2"
    testExpr "1 yd^3 : ft^3" "27 ft^3"

  describe "Compound conversions" $ do
    testExpr "1 mi/hr : in/min" "1056 in/min"
    testExpr "10 hp ~= 7456.9988186291 W" 1

  describe "Derived conversions" $ do
    testExpr "2 L : um^3" "2000000000000000 um^3"
    testExpr "2000 cm^3 : L" "2 L"
    testExpr "1 acre : yd^2" "4840 yd^2"

  describe "Precision loss" $ do
    testExpr "((2 ft : in) : ft) : in" "24 in"

testScalars :: SpecWith ()
testScalars = do
  describe "Simple comparisons" $ do
    testExpr "1 == 1" 1
    testExpr "1 /= 0" 1
    testExpr "1 <= 1" 1
    testExpr "1 >= 1" 1
    testExpr "1 < 2" 1
    testExpr "1 > 0" 1
    testExpr "1 /= 1" 0
    testExpr "1 == 0" 0
    testExpr "1 > 1" 0
    testExpr "1 < 1" 0
    testExpr "1 > 2" 0
    testExpr "1 < 0" 0

  describe "Epsilon comparisons" $ do
    testExpr "1 m ~= 3.28083990 ft" 1

  describe "Comparisons with units" $ do
    testExpr "1 ft == 1 ft" 1
    testExpr "1 ft < 2 ft" 1
    testExpr "1 ft < 1 m" 1
    testExpr "2 ft > 12 in" 1
    testExpr "3 ft == 1 yd" 1

  describe "Addition/subtraction" $ do
    testExpr "1 + 2" 3
    testExpr "1 ft + 2" "3 ft"
    testExpr "1 + 2 ft" "3 ft"
    testExpr "1 ft + 1 yd" "4 ft"
    testExpr "1 yd + 3 ft" "2 yd"
    testExpr "2 - 1" 1
    testExpr "2 ft - 1" "1 ft"
    testExpr "2 - 1 ft" "1 ft"
    testExpr "2 ft - 12 in" "1 ft"
    testExpr "24 in - 1 ft" "12 in"

  describe "Multiplication/division" $ do
    testExpr "10 ft * 1 yd" "30 ft^2"

testScript :: SpecWith ()
testScript = do
  describe "Custom units" $ do
    testWithScope "10 ball" "10 ball"

  describe "Simple expressions" $ do
    testWithScope "2 box * 10 ball/box" "20 ball"
