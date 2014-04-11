module Data.Quantities.ConstructorsSpec (spec) where

import Data.Quantities.Constructors
import Data.Quantities.Data

import Test.Hspec

{-# ANN module "HLint: ignore Redundant do" #-}

makeRightQuant :: Double -> [SimpleUnit] -> Either QuantityError Quantity
makeRightQuant m u = Right (baseQuant m u)

spec :: Spec
spec = do
  describe "fromString" $ do
    let m   = SimpleUnit "meter" "" 1
        m2  = SimpleUnit "meter" "" 2
        s   = SimpleUnit "second" "" 1
        ft  = SimpleUnit "foot" "" 1
        sn1 = SimpleUnit "second" "" (-1)
    it "parses numbers" $ do
      fromString "1"  `shouldBe` makeRightQuant 1  []
      fromString "-2" `shouldBe` makeRightQuant (-2) []
      fromString "1e3"  `shouldBe` makeRightQuant 1e3  []
      fromString "-1e3"  `shouldBe` makeRightQuant (-1e3)  []

    it "parses addition" $ do
      fromString "1+1"  `shouldBe` makeRightQuant 2  []

    it "parses units" $ do
      fromString "m"  `shouldBe` makeRightQuant 1 [m]
      fromString "-m"  `shouldBe` makeRightQuant (-1) [m]

    it "parses multiple units" $ do
      fromString "m*s"  `shouldBe` makeRightQuant 1 [m, s]

    it "parses division" $ do
      fromString "m/s"  `shouldBe` makeRightQuant 1 [m, s { power = -1 }]

    it "parses implicit multiplication" $ do
      fromString "ft sec"  `shouldBe` makeRightQuant 1 [ft, s]
      fromString "(ft) -sec"  `shouldBe` makeRightQuant (-1) [ft, s]

    it "parses exponentiation" $ do
      fromString "m^2" `shouldBe` makeRightQuant 1 [m2]
      fromString "m**2" `shouldBe` makeRightQuant 1 [m2]

    it "parses complex expressions" $ do
      fromString "100m*ft/s"  `shouldBe` makeRightQuant 100 [m, ft, sn1]
      fromString "(50 m) / s"  `shouldBe` makeRightQuant 50 [m, sn1]

    it "handles conversions" $ do
      fromString "s + min" `shouldBe` makeRightQuant 61 [s]
      fromString "ft + 12in" `shouldBe` makeRightQuant 2 [ft]
      fromString "ft - 12in" `shouldBe` makeRightQuant 0 [ft]

    it "throws DimensionalityErrors" $ do
      fromString "1 + m" `shouldSatisfy` isLeftDimError
      fromString "N + s" `shouldSatisfy` isLeftDimError

    it "performs unit conversions" $ do
      fromString "min => s " `shouldBe` makeRightQuant 60 [s]
      fromString "2 min + 15 s => s" `shouldBe` makeRightQuant 135 [s]

    it "catches unit conversion errors" $ do
      fromString "ft => UNDEF" `shouldBe` Left (UndefinedUnitError "UNDEF")


isLeftDimError :: Either QuantityError a -> Bool
isLeftDimError (Left (DimensionalityError _ _)) = True
isLeftDimError _ = False
