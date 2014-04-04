module Data.Quantities.DefinitionsSpec (spec) where

import qualified Data.Map as M

import Data.Quantities.Data (Definition(..), Definitions(..), SimpleUnit(..), baseQuant)
import Data.Quantities.Definitions
import Test.Hspec

{-# ANN module "HLint: ignore Redundant do" #-}

spec :: Spec
spec = do
    let baseDef  = BaseDefinition "meter" "length" ["m"]
        (Right baseDict) = makeDefinitions [baseDef]
        preDef  = PrefixDefinition "milli" 1e-3 ["m"]
        (Right preDict) = makeDefinitions [preDef]
        ftQuant = baseQuant 3.21 [SimpleUnit "m" "" 1]
        ftDef = UnitDefinition "foot" ftQuant ["ft", "feet"]
        (Right ftDict) = makeDefinitions [baseDef, ftDef]
        (Right allDict) = makeDefinitions [baseDef, preDef, ftDef]

    describe "makeDefinitions" $ do
      it "makes base definition" $ do
        bases baseDict M.! "meter" `shouldBe` (1, [SimpleUnit "meter" "" 1])
        unitsList baseDict  `shouldBe` ["meter", "m"]
        synonyms baseDict M.! "m" `shouldBe` "meter"
        unitTypes baseDict M.! "meter" `shouldBe` "length"

      it "makes prefix definition" $ do
        prefixes preDict `shouldBe` ["milli", "m"]
        prefixValues preDict M.! "milli" `shouldBe` 1e-3
        prefixSynonyms preDict M.! "m" `shouldBe` "milli"

      it "makes unit definition" $ do
        bases allDict M.! "foot" `shouldBe` (3.21, [SimpleUnit "meter" "" 1])
        synonyms ftDict M.! "ft" `shouldBe` "foot"
        "foot" `elem` unitsList ftDict `shouldBe` True

      describe "Eq Definitions" $ do
        let d1 = readDefinitions "m = [length]"
            d2 = readDefinitions "s = [time]"
        it "correctly equates same definitions" $ do
          d1 == d1 `shouldBe` True
          d2 == d2 `shouldBe` True
        it "correctly doesn't equate different definitions" $ do
          d1 == d2 `shouldBe` False
