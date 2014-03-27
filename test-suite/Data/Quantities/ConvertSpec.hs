module Data.Quantities.ConvertSpec (spec) where

import Data.Quantities.Convert
import Data.Quantities.Data (fromDefinitions, SimpleUnit(..), Quantity(..),
                             CompositeUnit, Definitions)
import Data.Quantities.DefinitionParser (readDefinitions)
import Data.Quantities.Definitions (makeDefinitions)
import Test.Hspec

{-# ANN module "HLint: ignore Redundant do" #-}

defaultQuant :: Double -> CompositeUnit -> Quantity
defaultQuant = fromDefinitions testDefs

testDefs :: Definitions
testDefs = makeDefinitions $ readDefinitions $ unlines [
  "milli- = 1e-3  = m-"
  ,"centi- = 1e-2  = c-"
  -- ,"kilo- =  1e3   = k-"
  ,"meter = [length] = m = metre"
  ,"inch = 2.54 * centimeter = in"
  ,"foot = 12 * inch = international_foot = ft = feet"
   ]

spec :: Spec
spec = do
    describe "convertBase" $ do
      it "converts to base" $ do
        let q = defaultQuant 1 [SimpleUnit "foot" "" 1]
        convertBase q `shouldBe` defaultQuant 0.3048 [SimpleUnit "meter" "" 1]

    describe "conversionFactor" $ do
      it "handles simple units" $ do
        let u1  = [SimpleUnit "meter" "" 1]
            u2  = [SimpleUnit "foot" "" 1]
            err = abs (conversionFactor testDefs u1 u2 - 3.280839)
        err < 0.0001 `shouldBe` True
      it "handles prefixes" $ do
        let u1 = [SimpleUnit "meter" "milli" 1]
            u2 = [SimpleUnit "meter" "" 1]
        conversionFactor testDefs u1 u2 `shouldBe` 1e-3

    describe "addQuants" $ do
      it "" $ do
        let q1 = defaultQuant 1 [SimpleUnit "foot" "" 1]
            q2 = defaultQuant 1 [SimpleUnit "meter" "" 1]
            q  = addQuants q1 q2
        abs (magnitude q - 4.280839) < 0.0001 `shouldBe` True
        units q `shouldBe` [SimpleUnit "foot" "" 1]

    describe "subtractQuants" $ do
      it "" $ do
        let q1 = defaultQuant 1 [SimpleUnit "foot" "" 1]
            q2 = defaultQuant 1 [SimpleUnit "meter" "" 1]
            q  = subtractQuants q1 q2
        abs (magnitude q - (-2.280839)) < 1e-5  `shouldBe` True
        units q `shouldBe` [SimpleUnit "foot" "" 1]
