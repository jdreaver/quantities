module Data.Quantities.ExprParserSpec (spec) where

import Data.Quantities.ExprParser
import Data.Quantities.Data (baseQuant, CompositeUnit, Quantity, SimpleUnit(..))
import Test.Hspec

{-# ANN module "HLint: ignore Redundant do" #-}

makeRightQuant :: Double -> CompositeUnit -> Either String Quantity
makeRightQuant m u = Right (baseQuant m u)

spec :: Spec
spec = do
    describe "readExpr" $ do
      it "parses numbers" $ do
        parseExprQuant "1"  `shouldBe` makeRightQuant 1  []
        parseExprQuant "-2" `shouldBe` makeRightQuant (-2) []
        parseExprQuant "1e3"  `shouldBe` makeRightQuant 1e3  []
        parseExprQuant "-1e3"  `shouldBe` makeRightQuant (-1e3)  []

      it "parses addition" $ do
        parseExprQuant "1+1"  `shouldBe` makeRightQuant 1  []

      it "parses units" $ do
        let m = SimpleUnit "m" "" 1
        parseExprQuant "m"  `shouldBe` makeRightQuant 1 [m]
        parseExprQuant "-m"  `shouldBe` makeRightQuant (-1) [m]

      it "parses multiple units" $ do
        let m = SimpleUnit "m" "" 1
            s = SimpleUnit "s" "" 1
        parseExprQuant "m*s"  `shouldBe` makeRightQuant 1 [m, s]

      it "parses division" $ do
        let m = SimpleUnit "m" "" 1
            s = SimpleUnit "s" "" 1
        parseExprQuant "m/s"  `shouldBe` makeRightQuant 1 [m, s { power = -1 }]

      it "parses implicit multiplication" $ do
        let ft = SimpleUnit "ft" "" 1
            sec = SimpleUnit "sec" "" 1
        parseExprQuant "ft sec"  `shouldBe` makeRightQuant 1 [ft, sec]
        parseExprQuant "(ft) -sec"  `shouldBe` makeRightQuant (-1) [ft, sec]

      it "parses exponentiation" $ do
        let m2 = SimpleUnit "m" "" 2
        parseExprQuant "m^2"  `shouldBe` makeRightQuant 1 [m2]
        parseExprQuant "m**2"  `shouldBe` makeRightQuant 1 [m2]

      it "parses complex expressions" $ do
        let m = SimpleUnit "m" "" 1
            ft = SimpleUnit "ft" "" 1
            sn1 = SimpleUnit "s" "" (-1)
        parseExprQuant "100m*ft/s"  `shouldBe` makeRightQuant 100 [m, ft, sn1]
        parseExprQuant "(50 m) / s"  `shouldBe` makeRightQuant 50 [m, sn1]
