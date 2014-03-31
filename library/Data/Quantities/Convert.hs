module Data.Quantities.Convert where

import Data.List (sort)
import qualified Data.Map as M

import Data.Quantities.Data (Quantity(..), CompositeUnit, SimpleUnit(..), Definitions(..)
                            ,multiplyQuants, QuantityError(..))

unityQuant :: Definitions -> Quantity
unityQuant = Quantity 1 []

-- | Convert quantity to given units.
--
-- >>> convert <$> fromString "m" <*> unitsFromString "ft"
-- Right (Right 3.280839895013123 foot)
convert :: Quantity -> CompositeUnit -> Either QuantityError Quantity
convert x = convert' (defs x) x


-- | Convert a quantity to its base units.
--
-- >>> convertBase <$> fromString "newton"
-- Right 1000.0 gram meter / second ** 2.0
convertBase :: Quantity -> Quantity
convertBase x = convertBase' (defs x) x


-- | Convert quantity to given units.
convert' :: Definitions -> Quantity -> CompositeUnit -> Either QuantityError Quantity
convert' d q us'
  | dimq /= dimus = Left $ DimensionalityError dimq dimus
  | otherwise     = Right $ Quantity (mb/mb') us' d
  where (Quantity mb  _ _) = convertBase' d q
        (Quantity mb' _ _) = toBase d us'
        dimq               = dimensionality' d (units q)
        dimus              = dimensionality' d us'


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
--
-- >>> dimensionality <$> fromString "newton"
-- Right [length,mass,time ** -2.0]
dimensionality :: Quantity -> CompositeUnit
dimensionality q = dimensionality' (defs q) (units q)

dimensionality' :: Definitions -> CompositeUnit -> CompositeUnit
dimensionality' d us = sort $ map dim ub
  where (Quantity _ ub _) = toBase d us
        dim (SimpleUnit sym _ pow) = SimpleUnit (unitTypes d M.! sym) "" pow


-- | Adds two quantities. Second quantity is converted to units of
-- first quantity.
addQuants :: Quantity -> Quantity -> Either QuantityError Quantity
addQuants = linearQuants (+)


-- | Subtract two quantities. Second quantity is converted to units of
-- first quantity.
subtractQuants :: Quantity -> Quantity -> Either QuantityError Quantity
subtractQuants = linearQuants (-)


linearQuants :: (Double -> Double -> Double) -> Quantity -> Quantity
                -> Either QuantityError Quantity
linearQuants f (Quantity m1 u1 d) q2 = case q of
  (Right q') -> Right $ Quantity (f m1 (magnitude q')) u1 d
  (Left err) -> Left err
  where q = convert q2 u1
