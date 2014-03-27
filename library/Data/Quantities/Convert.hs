module Data.Quantities.Convert where

import Data.List (sort)
import qualified Data.Map as M

import Data.Quantities.Data (Quantity(..), CompositeUnit, SimpleUnit(..), Definitions(..)
                            ,multiplyQuants)

unityQuant :: Definitions -> Quantity
unityQuant = Quantity 1 []

-- | Convert quantity to given units.
convert :: Quantity -> CompositeUnit -> Quantity
convert x = convert' (defs x) x


-- | Convert a quantity to its base units.
convertBase :: Quantity -> Quantity
convertBase x = convertBase' (defs x) x


-- | Convert quantity to given units.
convert' :: Definitions -> Quantity -> CompositeUnit -> Quantity
convert' d q us' = Quantity (mb/mb') us' d
  where (Quantity mb  _ _) = convertBase' d q
        (Quantity mb' _ _) = toBase d us'


-- | Convert a quantity to its base units.
convertBase' :: Definitions -> Quantity -> Quantity
convertBase' d (Quantity m us _) = Quantity (m*mb) ub d
  where (Quantity mb ub _) = toBase d us


-- | Converts a composite unit to its base quantity
toBase :: Definitions -> CompositeUnit -> Quantity
toBase d = foldr (multiplyQuants . simpleToBase d) (unityQuant d)


-- | Converts a simple unit to its base quantity.
simpleToBase :: Definitions -> SimpleUnit -> Quantity
simpleToBase d (SimpleUnit sym pre pow) = Quantity m us d
  where (m', u') = bases d M.! sym
        us = map (\(SimpleUnit s p pow') -> SimpleUnit s p (pow*pow')) u'
        m = (m' * (prefixValues d M.! pre)) ** pow


-- | Computes dimensionality of quantity.
dimensionality :: Quantity -> CompositeUnit
dimensionality q = dimensionality' (defs q) (units q)

dimensionality' :: Definitions -> CompositeUnit -> CompositeUnit
dimensionality' d us = sort $ map dim ub
  where (Quantity _ ub _) = toBase d us
        dim (SimpleUnit sym _ pow) = SimpleUnit (unitTypes d M.! sym) "" pow


-- | Adds two quantities.
addQuants :: Quantity -> Quantity -> Quantity
addQuants (Quantity m1 u1 d) q2 = Quantity (m1+m2') u1 d
  where (Quantity m2' _ _) = convert q2 u1


-- | Subtract two quantities.
subtractQuants :: Quantity -> Quantity -> Quantity
subtractQuants (Quantity m1 u1 d) q2 = Quantity (m1-m2') u1 d
  where (Quantity m2' _ _) = convert q2 u1
