module Data.Quantities.ConvertSpec (spec) where

import Data.Quantities.Convert
import Data.Quantities.Data (fromDefinitions, SimpleUnit(..), Quantity(..),
                             CompositeUnit)
import Data.Quantities.DefaultUnits (defaultDefinitions)
import Test.Hspec

{-# ANN module "HLint: ignore Redundant do" #-}

defaultQuant :: Double -> CompositeUnit -> Quantity
defaultQuant = fromDefinitions defaultDefinitions

spec :: Spec
spec = do
    describe "convertBase" $ do
      it "converts to base" $ do
        let q = defaultQuant 1 [SimpleUnit "foot" "" 1]
        convertBase q `shouldBe` defaultQuant (1/3.280839) [SimpleUnit "meter" "" 1]

    describe "conversionFactor" $ do
      it "" $ do
        let u1 = [SimpleUnit "meter" "" 1]
            u2 = [SimpleUnit "foot" "" 1]
        conversionFactor defaultDefinitions u1 u2 `shouldBe` 3.280839

    describe "addQuants" $ do
      it "" $ do
        let q1 = defaultQuant 1 [SimpleUnit "foot" "" 1]
            q2 = defaultQuant 1 [SimpleUnit "meter" "" 1]
        addQuants q1 q2 `shouldBe` defaultQuant 4.280839 [SimpleUnit "foot" "" 1]

    describe "subtractQuants" $ do
      it "" $ do
        let q1 = defaultQuant 1 [SimpleUnit "foot" "" 1]
            q2 = defaultQuant 1 [SimpleUnit "meter" "" 1]
        subtractQuants q1 q2 `shouldBe` defaultQuant (-2.280839) [SimpleUnit "foot" "" 1]
