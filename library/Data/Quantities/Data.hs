-- | Base module for all data structures.
module Data.Quantities.Data where

import Data.List (partition, sort)
import qualified Data.Map as M

-- | String representation of a unit. Examples: "meter", "foot"
type Symbol = String

-- | Representation of single unit. For example: \"mm^2\" is
-- represented as
--
-- > SimpleUnit { symbol = "meter", prefix = "milli", power = 2.0 }
data SimpleUnit = SimpleUnit { symbol :: String
                             , prefix :: String
                             , power  :: Double } deriving (Eq, Ord)

instance Show SimpleUnit where
  show (SimpleUnit s pr p)
    | p == 1    = sym
    | otherwise = sym ++ " ** " ++ show p
    where sym = pr ++ s


data CompoundUnit = CompoundUnit { defs   :: Definitions
                                 , sUnits :: [SimpleUnit]
                                 } deriving (Eq, Ord)

instance Show CompoundUnit where
  show (CompoundUnit _ us) = unwords . map showCompUnit' $ showSort us


-- | Show a single unit, but prepend with '/' if negative
showCompUnit' :: SimpleUnit -> String
showCompUnit' (SimpleUnit s pr p)
  | p == 1    = sym
  | p == -1   = "/ " ++ sym
  | p < 0     = "/ " ++ sym ++ " ** " ++ show (-p)
  | otherwise = sym ++ " ** " ++ show p
  where sym = pr ++ s


-- | Combination of magnitude and units.
data Quantity = Quantity { magnitude :: Double
                           -- ^ Numerical magnitude of quantity.
                           --
                           -- >>> magnitude <$> fromString "100 N * m"
                           -- Right 100.0
                         , units     :: CompoundUnit
                           -- ^ Units associated with quantity.
                           --
                           -- >>> units <$> fromString "3.4 m/s^2"
                           -- Right [meter,second ** -2.0]
                         } deriving (Ord)


-- | Convenience function to extract SimpleUnit collection from Quantity's
-- CompoundUnit.
units' :: Quantity -> [SimpleUnit]
units' = sUnits . units

-- | Convenience function to extract Definitions from Quantity's CompoundUnit.
defs' :: Quantity -> Definitions
defs' = defs . units

instance Show Quantity where
  show (Quantity m us) = show m ++ " " ++ show us


-- | Convenience function to make quantity with no definitions.
baseQuant :: Double -> [SimpleUnit] -> Quantity
baseQuant m us = Quantity m (CompoundUnit emptyDefinitions us)

-- | Sort units but put negative units at end.
showSort :: [SimpleUnit] -> [SimpleUnit]
showSort c = pos ++ neg
  where (pos, neg) = partition (\q -> power q > 0) c

instance Eq Quantity where
  (Quantity m1 u1) == (Quantity m2 u2) = m1 == m2 && sort (sUnits u1) == sort (sUnits u2)


-- | Custom error type
data QuantityError = UndefinedUnitError String
                     -- ^ Used when trying to parse an undefined unit.
                   | DimensionalityError CompoundUnit CompoundUnit
                     -- ^ Used when converting units that do not have the same
                     -- dimensionality (example: convert meter to second).
                   | UnitAlreadyDefinedError String
                     -- ^ Used internally when defining units and a unit is
                     -- already defined.
                   | PrefixAlreadyDefinedError String
                     -- ^ Used internally when defining units and a prefix is
                     -- already defined.
                   | ParserError String
                     -- ^ Used when a string cannot be parsed.
                   | DifferentDefinitionsError CompoundUnit CompoundUnit
                     -- ^ Used when two quantities come from different
                     -- Definitions.
                   deriving (Show, Eq)


-- | Useful for monadic computations with 'QuantityError's. Some examples:
--
-- > computation :: QuantityComputation Quantity
-- > computation = do
-- >   x <- fromString "mile/hr"
-- >   y <- unitsFromString "m/s"
-- >   convert x y
--
-- Returns @Right 0.44704 meter / second@
--
-- > computation :: QuantityComputation Quantity
-- > computation = do
-- >   x <- fromString "BADUNIT"
-- >   convertBase x
--
-- Returns @Left (UndefinedUnitError "BADUNIT")@

type QuantityComputation = Either QuantityError

-- | Combines equivalent units and removes units with powers of zero.
reduceUnits :: Quantity -> Quantity
reduceUnits q = q { units = newUnits }
  where newUnits = (units q) { sUnits = reduceUnits' (units' q) }

reduceUnits', removeZeros :: [SimpleUnit] -> [SimpleUnit]
reduceUnits'  = removeZeros . reduceComp . sort
  where reduceComp [] = []
        reduceComp (SimpleUnit x pr1 p1 : SimpleUnit y pr2 p2: xs)
          | (x,pr1) == (y,pr2) = SimpleUnit x pr1 (p1+p2) : reduceComp xs
          | otherwise = SimpleUnit x pr1 p1 : reduceComp (SimpleUnit y pr2 p2 : xs)
        reduceComp (x:xs) = x : reduceComp xs


removeZeros [] = []
removeZeros (SimpleUnit _ _ 0.0 : xs) = removeZeros xs
removeZeros (x:xs) = x : removeZeros xs

invertUnits :: [SimpleUnit] -> [SimpleUnit]
invertUnits = map invertSimpleUnit

-- | Inverts unit by negating the power field.
invertSimpleUnit :: SimpleUnit -> SimpleUnit
invertSimpleUnit (SimpleUnit s pr p) = SimpleUnit s pr (-p)

-- | Multiplies two quantities.
multiplyQuants :: Quantity -> Quantity -> Quantity
multiplyQuants x y = reduceUnits $ Quantity mag newUnits
  where mag      = magnitude x * magnitude y
        newUnits = (units x) { sUnits = units' x ++ units' y }

-- | Divides two quantities.
divideQuants :: Quantity -> Quantity -> Quantity
divideQuants x y = reduceUnits $ Quantity mag newUnits
  where mag      = magnitude x / magnitude y
        newUnits = (units x) { sUnits = units' x ++ invertUnits (units' y) }

-- | Exponentiates a quantity with a double.
exptQuants :: Quantity -> Double -> Quantity
exptQuants (Quantity x u) y = reduceUnits $ Quantity (x**y) newUnits
  where expUnits = map (\(SimpleUnit s pr p) -> SimpleUnit s pr (p*y))
        newUnits = u { sUnits = expUnits (sUnits u) }


data Definition = PrefixDefinition { defPrefix   :: Symbol
                                   , factor      :: Double
                                   , defSynonyms :: [Symbol]}
                | BaseDefinition   { base        :: Symbol
                                   , dimBase     :: Symbol
                                   , defSynonyms ::[Symbol]}
                | UnitDefinition   { defSymbol   :: Symbol
                                   , quantity    :: Quantity
                                   , defSynonyms :: [Symbol]} deriving (Show, Eq, Ord)


data Definitions = Definitions { bases          :: M.Map String (Double, [SimpleUnit])
                                 -- ^ Map from symbol to base units and
                                 -- conversion factor to those units.
                               , synonyms       :: M.Map String String
                                 -- ^ Map from alias to symbol. Symbols without
                                 -- aliases are present as identity maps.
                               , unitsList      :: [String]
                                 -- ^ List of all units (no aliases). Used in
                                 -- prefix parser, and to detect duplicate
                                 -- definitions.
                               , prefixes       :: [String]
                                 -- ^ List of all prefixes (no aliases). Used
                                 -- in prefix parser, and to detect duplicate
                                 -- prefix definitions.
                               , prefixValues   :: M.Map String Double
                                 -- ^ Multiplicative factor of prefixes.
                               , prefixSynonyms :: M.Map String String
                                 -- ^ Map from prefix alias to prefix.
                               , unitTypes      :: M.Map String String
                                 -- ^ Map from base symbols to unit types.
                               , defStringHash  :: Int
                                 -- ^ Hash of the definitions string used to
                                 -- create definitions. Defaults to -1 if
                                 -- modified or no string was used.
                               } deriving (Show, Ord)

instance Eq Definitions where
  d1 == d2 = defStringHash d1 == defStringHash d2

emptyDefinitions :: Definitions
emptyDefinitions = Definitions { bases          = M.empty
                               , synonyms       = M.empty
                               , unitsList      = []
                               , prefixes       = []
                               , prefixValues   = M.fromList [("", 1)]
                               , prefixSynonyms = M.fromList [("", "")]
                               , unitTypes      = M.empty
                               , defStringHash  = -1 }


-- | Combine two Definitions structures
unionDefinitions :: Definitions -> Definitions -> Definitions
unionDefinitions d1 d2 = Definitions {
  bases = bases d1 `M.union` bases d2
  , synonyms = synonyms d1 `M.union` synonyms d2
  , unitsList = unitsList d1 ++ unitsList d2
  , prefixes = prefixes d1 ++ prefixes d2
  , prefixValues = prefixValues d1 `M.union` prefixValues d2
  , prefixSynonyms = prefixSynonyms d1 `M.union` prefixSynonyms d2
  , unitTypes = unitTypes d1 `M.union` unitTypes d2
  , defStringHash = -1 }
