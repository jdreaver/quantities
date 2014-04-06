module Data.Quantities.Convert where

import Data.List (sort)
import qualified Data.Map as M

import Data.Quantities.Data

unityQuant :: Definitions -> Quantity
unityQuant d = Quantity 1 (CompoundUnit d [])

-- | Convert quantity to given units.
--
-- >>> convert <$> fromString "m" <*> unitsFromString "ft"
-- Right (Right 3.280839895013123 foot)
convert :: Quantity -> CompoundUnit -> Either QuantityError Quantity
convert q us
  | hq /= hus = Left  $ DifferentDefinitionsError (units q) us
  | otherwise = convert' (defs us) q us
  where hq  = defStringHash (defs' q)
        hus = defStringHash (defs us)


-- | Convert a quantity to its base units.
--
-- >>> convertBase <$> fromString "newton"
-- Right 1000.0 gram meter / second ** 2.0
convertBase :: Quantity -> Quantity
convertBase x = convertBase' (defs' x) x


-- | Convert quantity to given units.
convert' :: Definitions -> Quantity -> CompoundUnit -> Either QuantityError Quantity
convert' d q us'
  | dimq /= dimus = Left  $ DimensionalityError (CompoundUnit d dimq) (CompoundUnit d dimus)
  | otherwise     = Right $ Quantity (mb/mb') us'
  where mb    = magnitude $ convertBase' d q
        mb'   = magnitude $ toBase d (sUnits us')
        dimq  = dimensionality' d (units' q)
        dimus = dimensionality' d (sUnits us')



-- | Convert a quantity to its base units.
convertBase' :: Definitions -> Quantity -> Quantity
convertBase' d (Quantity m us) = Quantity (m*mb) ub
  where (Quantity mb ub) = toBase d (sUnits us)


-- | Converts a composite unit to its base quantity
toBase :: Definitions -> [SimpleUnit] -> Quantity
toBase d = foldr (multiplyQuants . simpleToBase d) (unityQuant d)


-- | Converts a simple unit to its base quantity.
simpleToBase :: Definitions -> SimpleUnit -> Quantity
simpleToBase d (SimpleUnit sym pre pow) = Quantity m (CompoundUnit d us)
  where (m', u') = bases d M.! sym
        us = map (\(SimpleUnit s p pow') -> SimpleUnit s p (pow*pow')) u'
        m = (m' * (prefixValues d M.! pre)) ** pow


-- | Computes dimensionality of quantity.
--
-- >>> dimensionality <$> fromString "newton"
-- Right [length,mass,time ** -2.0]
dimensionality :: Quantity -> CompoundUnit
dimensionality q = CompoundUnit (defs' q) dimUnits
  where dimUnits = dimensionality' (defs' q) (units' q)

dimensionality' :: Definitions -> [SimpleUnit] -> [SimpleUnit]
dimensionality' d us = sort $ map dim ub
  where ub = units' $ toBase d us
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
linearQuants f (Quantity m1 u1) q2 = case q of
  (Right q') -> Right $ Quantity (f m1 (magnitude q')) u1
  (Left err) -> Left err
  where q = convert q2 u1
