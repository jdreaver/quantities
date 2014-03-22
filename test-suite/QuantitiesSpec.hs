module QuantitiesSpec (spec) where

import Quantities (quantities)
import Test.Hspec
import Test.Hspec.QuickCheck

spec :: Spec
spec = do
    describe "quantities" $ do
        it "returns the unit value" $ do
            quantities `shouldBe` ()

        prop "equals the unit value" $
            \ x -> quantities == x
