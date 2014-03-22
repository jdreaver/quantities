module Data.Quantities.DataSpec (spec) where

import Data.Quantities.Data
import Test.Hspec

{-# ANN module "HLint: ignore Redundant do" #-}

mm2, s :: SimpleUnit
mm2 = SimpleUnit "meter" "milli" 2
s = SimpleUnit "second" "" 1

mm2quant, squant :: Quantity
mm2quant = baseQuant 4 [mm2]
squant = baseQuant 2 [s]


testQuant :: Quantity
testQuant = baseQuant 5 [mm2]

spec :: Spec
spec = do
    describe "Unit creation" $ do
      it "compiles" $ do
        symbol mm2 `shouldBe` "meter"
        prefix mm2 `shouldBe` "milli"

    describe "Quantity creation" $ do
      it "compiles" $ do
        magnitude testQuant `shouldBe` 5.0
        units testQuant `shouldBe` [mm2]

    describe "Reduce units" $ do
      it "should work" $ do
        (reduceUnits . replicate 2) mm2 `shouldBe` [mm2 {power=4}]

    describe "invertSimpleUnit" $ do
      it "should put unit in denominator" $ do
        power (invertSimpleUnit mm2) `shouldBe` -2.0

    describe "multiplyQuants" $ do
      it "multiplies two quantities" $ do
        multiplyQuants mm2quant squant `shouldBe` baseQuant 8 [mm2, s]

    describe "divideQuants" $ do
      it "divides two quantities" $ do
        let invs = s { power = -1 }
        divideQuants mm2quant squant `shouldBe` baseQuant 2 [mm2, invs]

    describe "expQuants" $ do
      it "exponentiate quantity" $ do
        let mm4 = mm2 { power = 4 }
        exptQuants mm2quant 2 `shouldBe` baseQuant 16 [mm4]
