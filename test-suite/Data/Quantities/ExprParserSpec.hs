module Data.Quantities.ExprParserSpec (spec) where

import Data.Either

import Data.Quantities.Constructors (d)
import Data.Quantities.Data
import Data.Quantities.Definitions
import Data.Quantities.ExprParser

import Test.Hspec

{-# ANN module "HLint: ignore Redundant do" #-}

isLeft :: Either a b -> Bool
isLeft = null . rights . return

spec :: Spec
spec = do
    let m2    = SimpleUnit "m" "" 2
        ppm2  = SimpleUnit "meter" "" 2
        mm2   = SimpleUnit "mm" "" 2
        ppmm2 = SimpleUnit "meter" "milli" 2
        bad   = SimpleUnit "asdfdsaf" "" 2

    describe "preprocessUnit" $ do
      it "handles base" $ do
        let (Right computed) = preprocessUnit d m2
        computed  `shouldBe` ppm2

      it "handles prefix" $ do
        let (Right computed) = preprocessUnit d mm2
        computed `shouldBe` ppmm2

      it "rejects bad unit" $ do
        let ret = preprocessUnit d bad
        isLeft ret `shouldBe` True

    let qm2 = baseQuant 3 [m2]
    describe "preprocessQuantity" $ do
      it "doesn't need own dict" $ do
        let (Right computed) = preprocessQuantity d qm2
        computed  `shouldBe` baseQuant 3 [ppm2]


    describe "prefixParser" $ do
      let hectDef    = PrefixDefinition "hecto" 1e-3 ["h"]
          hrDef      = BaseDefinition "hour" "time" ["h", "hr"]
          (Right hectHrDict) = makeDefinitions [hectDef, hrDef]
      it "handles hecto/hr ambiguity" $ do
        let (pr, sym) = prefixParser hectHrDict "hr"
        pr `shouldBe` ""
        sym `shouldBe` "hr"

      let milliDef    = PrefixDefinition "milli" 1e-3 ["m"]
          inchDef     = BaseDefinition "inch" "length" ["in"]
          minDef      = BaseDefinition "minute" "time" ["min"]
          (Right inchMinDict) = makeDefinitions [milliDef, inchDef, minDef]
      it "handles min/milliinch ambiguity" $ do
        let (pr, sym) = prefixParser inchMinDict "min"
        pr `shouldBe` ""
        sym `shouldBe` "min"
