module Data.Quantities.ConvertSpec (spec) where

import Data.Quantities.Convert
import Data.Quantities.Data
import Data.Quantities.Definitions (readDefinitions)
import Test.Hspec

{-# ANN module "HLint: ignore Redundant do" #-}

defaultQuant :: Double -> [SimpleUnit] -> Quantity
defaultQuant m us = Quantity m (defaultUnits us)

defaultUnits :: [SimpleUnit] -> CompoundUnit
defaultUnits = CompoundUnit testDefs

testDefs :: Definitions
(Right testDefs) = readDefinitions testDefsString

testDefsString :: String
testDefsString = unlines [
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
            u2  = defaultUnits [SimpleUnit "foot" "" 1]
            (Right conv) = convert q1 u2
            err = abs (magnitude conv - 3.280839)
        err < 0.0001 `shouldBe` True
      it "handles prefixes" $ do
        let q1 = defaultQuant 1 [SimpleUnit "meter" "milli" 1]
            u2 = defaultUnits [SimpleUnit "meter" "" 1]
            (Right conv) = convert q1 u2
        magnitude conv `shouldBe` 1e-3
      it "throws dimensionality error" $ do
        let q1  = defaultQuant 1 [SimpleUnit "second" "" 1]
            u2  = defaultUnits [SimpleUnit "meter" "" 1]
            dq1 = defaultUnits $ dimensionality' testDefs (units' q1)
            du2 = defaultUnits $ dimensionality' testDefs (sUnits u2)
            (Left err) = convert q1 u2
        err `shouldBe` DimensionalityError dq1 du2

      it "fails when definitions different" $ do
        let q = defaultQuant 1 [SimpleUnit "second" "" 1]
            (Right d) = readDefinitions $ testDefsString ++ "\nfun = [fun]"
            us  = units $ defaultQuant 1 [SimpleUnit "minute" "" 1]
            usd = us { defs = d }
            (Left expect) = convert q usd
        expect `shouldBe` DifferentDefinitionsError (units q) usd

    describe "addQuants" $ do
      it "" $ do
        let q1 = defaultQuant 1 [SimpleUnit "foot" "" 1]
            q2 = defaultQuant 1 [SimpleUnit "meter" "" 1]
            (Right q)  = addQuants q1 q2
        abs (magnitude q - 4.280839) < 0.0001 `shouldBe` True
        units q `shouldBe` defaultUnits [SimpleUnit "foot" "" 1]

    describe "subtractQuants" $ do
      it "" $ do
        let q1 = defaultQuant 1 [SimpleUnit "foot" "" 1]
            q2 = defaultQuant 1 [SimpleUnit "meter" "" 1]
            (Right q)  = subtractQuants q1 q2
        abs (magnitude q - (-2.280839)) < 1e-5  `shouldBe` True
        units q `shouldBe` defaultUnits [SimpleUnit "foot" "" 1]

    describe "dimensionality" $ do
      it "computes dimensionality of simple units" $ do
        let ft = defaultQuant 1 [SimpleUnit "foot" "" 1]
        dimensionality ft `shouldBe` defaultUnits  [SimpleUnit "[length]" "" 1]

      it "computes dimensionality of complex units" $ do
        let sft = defaultQuant 1 [SimpleUnit "second" "" 1, SimpleUnit "foot" "" 1]
            expect = defaultUnits [SimpleUnit "[length]" "" 1, SimpleUnit "[time]" "" 1]
        dimensionality sft `shouldBe` expect
