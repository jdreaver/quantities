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
            -- m = defaultQuant [SimpleUnit "meter" "" 1]
        convertBase q `shouldBe` defaultQuant 3.280839 [SimpleUnit "meter" "" 1]
