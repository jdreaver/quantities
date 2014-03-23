module Data.Quantities.DefinitionsSpec (spec) where

import qualified Data.Map as M

import Data.Quantities.Data (Definition(..), Definitions(..), SimpleUnit(..),
                             baseQuant)
import Data.Quantities.Definitions
import Test.Hspec

{-# ANN module "HLint: ignore Redundant do" #-}

spec :: Spec
spec = do
    let baseDef  = BaseDefinition "meter" "length" ["m"]
        baseDict = makeDefinitions [baseDef]
        preDef  = PrefixDefinition "milli" 1e-3 ["m"]
        preDict = makeDefinitions [preDef]
        ftQuant = baseQuant 3.21 [SimpleUnit "m" "" 1]
        ftDef = UnitDefinition "foot" ftQuant ["ft", "feet"]
        ftDict = makeDefinitions [baseDef, ftDef]
        allDict = makeDefinitions [baseDef, preDef, ftDef]

    describe "makeDefinitions" $ do
      it "makes base definition" $ do
        bases baseDict M.! "meter" `shouldBe` "meter"
        conversions baseDict M.! ("meter", "meter") `shouldBe` 1
        unitsList baseDict  `shouldBe` ["meter", "m"]
        synonyms baseDict M.! "m" `shouldBe` "meter"
        unitTypes baseDict M.! "meter" `shouldBe` "length"

      it "makes prefix definition" $ do
        prefixes preDict `shouldBe` ["milli", "m"]
        prefixValues preDict M.! "milli" `shouldBe` 1e-3
        prefixSynonyms preDict M.! "m" `shouldBe` "milli"

      it "makes unit definition" $ do
        bases ftDict M.! "foot" `shouldBe` "meter"
        conversions ftDict M.! ("foot", "meter") `shouldBe` 3.21
        synonyms ftDict M.! "ft" `shouldBe` "foot"
        "foot" `elem` unitsList ftDict `shouldBe` True

    let m2    = SimpleUnit "m" "" 2
        ppm2  = SimpleUnit "meter" "" 2
        mm2   = SimpleUnit "mm" "" 2
        ppmm2 = SimpleUnit "meter" "milli" 2

    describe "preprocessUnit" $ do
      it "handles base" $ do
        preprocessUnit baseDict m2 `shouldBe` ppm2

      it "handles prefix" $ do
        preprocessUnit allDict mm2 `shouldBe` ppmm2

    let qm2 = baseQuant 3 [m2]
    describe "preprocessQuantity" $ do
      it "doesn't need own dict" $ do
        preprocessQuantity allDict qm2 `shouldBe` baseQuant 3 [ppm2]
