module Data.Quantities.DefinitionParserSpec (spec) where

import Data.Quantities.Data (Definition(..), SimpleUnit(..), baseQuant)
import Data.Quantities.DefinitionParser
import Test.Hspec

{-# ANN module "HLint: ignore Redundant do" #-}

spec :: Spec
spec = do
    describe "readDefinitions" $ do
      let mLine    = "milli- = 1e-3 = m-"
          milli    = head $ readDefinitions mLine
          milliDef = PrefixDefinition "milli" 1e-3 ["m"]

      it "read prefix definition" $ do
        milli `shouldBe` milliDef


      let lenLine = "meter = [length] = m"
          len     = head $ readDefinitions lenLine
          lenDef  = BaseDefinition "meter" "length" ["m"]

      it "read base definition" $ do
        len `shouldBe` lenDef


      let feetLine = "foot = 3.21 m = ft = feet"
          feet     = head $ readDefinitions feetLine
          feetDef  = UnitDefinition "foot" q ["ft", "feet"]
          q        = baseQuant 3.21 [SimpleUnit "m" "" 1]

      it "read unit definition" $ do
        feet `shouldBe` feetDef


      let allLines = unlines [mLine, lenLine, feetLine]
          allDefs  = [milliDef, lenDef, feetDef]

      it "read multiple definitions" $ do
        readDefinitions allLines `shouldBe` allDefs
