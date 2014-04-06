module Data.QuantitiesSpec (spec) where

import Data.Quantities
import Data.Quantities.Data
import Test.Hspec

{-# ANN module "HLint: ignore Redundant do" #-}

spec :: Spec
spec = do
  describe "parseQuant" $ do
    it "parses simple unit" $ do
      let (Right m) = fromString "m"
      magnitude m `shouldBe` 1
      units' m `shouldBe` [SimpleUnit "meter" "" 1]

  describe "parseUnit" $ do
    it "parses simple unit" $ do
      let (Right m) = unitsFromString "m"
      sUnits m `shouldBe` [SimpleUnit "meter" "" 1]
