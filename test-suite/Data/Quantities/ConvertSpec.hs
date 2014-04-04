module Data.Quantities.ConvertSpec (spec) where

import Data.Quantities.Convert
import Data.Quantities.Data (fromDefinitions, SimpleUnit(..), Quantity(..),
                             CompositeUnit, Definitions, QuantityError(..))
import Data.Quantities.Definitions (readDefinitions)
import Test.Hspec

{-# ANN module "HLint: ignore Redundant do" #-}

defaultQuant :: Double -> CompositeUnit -> Quantity
defaultQuant = fromDefinitions testDefs

testDefs :: Definitions
(Right testDefs) = readDefinitions $ unlines [
  "milli- = 1e-3  = m-"
  ,"centi- = 1e-2  = c-"
  -- ,"kilo- =  1e3   = k-"
  ,"meter = [length] = m = metre"
  ,"inch = 2.54 * centimeter = in"
  ,"foot = 12 * inch = international_foot = ft = feet"
  ,"second = [time] = s = sec"
   ]

spec :: Spec
spec = do
    describe "convertBase" $ do
      it "converts to base" $ do
        let q = defaultQuant 1 [SimpleUnit "foot" "" 1]
        convertBase q `shouldBe` defaultQuant 0.3048 [SimpleUnit "meter" "" 1]

    describe "convert" $ do
      it "handles simple units" $ do
        let q1  = defaultQuant 1 [SimpleUnit "meter" "" 1]
            u2  = [SimpleUnit "foot" "" 1]
            (Right conv) = convert q1 u2
            err = abs (magnitude conv - 3.280839)
        err < 0.0001 `shouldBe` True
      it "handles prefixes" $ do
        let q1 = defaultQuant 1 [SimpleUnit "meter" "milli" 1]
            u2 = [SimpleUnit "meter" "" 1]
            (Right conv) = convert q1 u2
        magnitude conv `shouldBe` 1e-3
      it "throws dimensionality error" $ do
        let q1  = defaultQuant 1 [SimpleUnit "second" "" 1]
            u2  = [SimpleUnit "meter" "" 1]
            dq1 = dimensionality' testDefs (units q1)
            du2 = dimensionality' testDefs u2
            (Left err) = convert q1 u2
        err `shouldBe` DimensionalityError dq1 du2

    describe "addQuants" $ do
      it "" $ do
        let q1 = defaultQuant 1 [SimpleUnit "foot" "" 1]
            q2 = defaultQuant 1 [SimpleUnit "meter" "" 1]
            (Right q)  = addQuants q1 q2
        abs (magnitude q - 4.280839) < 0.0001 `shouldBe` True
        units q `shouldBe` [SimpleUnit "foot" "" 1]

    describe "subtractQuants" $ do
      it "" $ do
        let q1 = defaultQuant 1 [SimpleUnit "foot" "" 1]
            q2 = defaultQuant 1 [SimpleUnit "meter" "" 1]
            (Right q)  = subtractQuants q1 q2
        abs (magnitude q - (-2.280839)) < 1e-5  `shouldBe` True
        units q `shouldBe` [SimpleUnit "foot" "" 1]

    describe "dimesionality" $ do
      it "computes dimensionality of simple units" $ do
        let ft = defaultQuant 1 [SimpleUnit "foot" "" 1]
        dimensionality ft `shouldBe` [SimpleUnit "length" "" 1]

      it "computes dimensionality of complex units" $ do
        let sft = defaultQuant 1 [SimpleUnit "second" "" 1, SimpleUnit "foot" "" 1]
        dimensionality sft `shouldBe` [SimpleUnit "length" "" 1, SimpleUnit "time" "" 1]
