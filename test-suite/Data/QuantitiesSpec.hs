module Data.QuantitiesSpec (spec) where

import Data.Quantities (quantities)
import Test.Hspec
import Test.Hspec.QuickCheck

{-# ANN module "HLint: ignore Redundant do" #-}

spec :: Spec
spec = do
    describe "quantities" $ do
        it "returns the unit value" $ do
            quantities `shouldBe` ()

        prop "equals the unit value" $
            \ x -> quantities == x
