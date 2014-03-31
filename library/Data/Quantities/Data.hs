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
                             , power  :: Double} deriving (Eq, Ord)

instance Show SimpleUnit where
  show (SimpleUnit s pr p)
    | p == 1    = sym
    | otherwise = sym ++ " ** " ++ show p
    where sym = pr ++ s


-- | Collection of SimpleUnits. Represents combination of simple
-- units.
type CompositeUnit = [SimpleUnit]

-- | Used to show composite units.
showCompUnit :: CompositeUnit -> String
showCompUnit = unwords . map showCompUnit' . showSort

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
                         , units     :: CompositeUnit
                           -- ^ Units associated with quantity.
                           --
                           -- >>> units <$> fromString "3.4 m/s^2"
                           -- Right [meter,second ** -2.0]
                         , defs      :: Definitions } deriving (Ord)


instance Show Quantity where
  show (Quantity m us _) = show m ++ " " ++ showCompUnit us

-- | Sort units but put negative units at end.
showSort :: CompositeUnit -> CompositeUnit
showSort c = pos ++ neg
  where (pos, neg) = partition (\q -> power q > 0) c

instance Eq Quantity where
  (Quantity m1 u1 _) == (Quantity m2 u2 _) = m1 == m2 && sort u1 == sort u2


-- | Quantity without definitions.
baseQuant :: Double -> CompositeUnit -> Quantity
baseQuant m u = Quantity m u emptyDefinitions

fromDefinitions :: Definitions -> Double -> CompositeUnit -> Quantity
fromDefinitions d m u = Quantity m u d

-- | Custom error type
data QuantityError = UndefinedUnitError String
                     -- ^ Used when trying to parse an undefined unit.
                   | DimensionalityError CompositeUnit CompositeUnit
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
                   deriving (Show, Eq)


-- | Computation monad that propagates 'QuantityError's. Some examples:
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
reduceUnits q = q { units = reduceUnits' (units q) }

reduceUnits', removeZeros :: CompositeUnit -> CompositeUnit
reduceUnits'  = removeZeros . reduceComp . sort
  where reduceComp [] = []
        reduceComp (SimpleUnit x pr1 p1 : SimpleUnit y pr2 p2: xs)
          | (x,pr1) == (y,pr2) = SimpleUnit x pr1 (p1+p2) : reduceComp xs
          | otherwise = SimpleUnit x pr1 p1 : reduceComp (SimpleUnit y pr2 p2 : xs)
        reduceComp (x:xs) = x : reduceComp xs


removeZeros [] = []
removeZeros (SimpleUnit _ _ 0.0 : xs) = removeZeros xs
removeZeros (x:xs) = x : removeZeros xs

invertUnits :: CompositeUnit -> CompositeUnit
invertUnits = map invertSimpleUnit

-- | Inverts unit by negating the power field.
invertSimpleUnit :: SimpleUnit -> SimpleUnit
invertSimpleUnit (SimpleUnit s pr p) = SimpleUnit s pr (-p)

-- | Multiplies two quantities.
multiplyQuants :: Quantity -> Quantity -> Quantity
multiplyQuants x y = reduceUnits $ Quantity mag newUnits (defs x)
  where mag      = magnitude x * magnitude y
        newUnits = units x ++ units y

-- | Divides two quantities.
divideQuants :: Quantity -> Quantity -> Quantity
divideQuants x y = reduceUnits $ Quantity mag newUnits (defs x)
  where mag      = magnitude x / magnitude y
        newUnits = units x ++ invertUnits (units y)

-- | Exponentiates a quantity with a double.
exptQuants :: Quantity -> Double -> Quantity
exptQuants (Quantity x u d) y = reduceUnits $ Quantity (x**y) (expUnits u) d
  where expUnits = map (\(SimpleUnit s pr p) -> SimpleUnit s pr (p*y))


data Definition = PrefixDefinition { defPrefix   :: Symbol
                                   , factor      :: Double
                                   , defSynonyms :: [Symbol]}
                | BaseDefinition   { base        :: Symbol
                                   , dimBase     :: Symbol
                                   , defSynonyms ::[Symbol]}
                | UnitDefinition   { defSymbol   :: Symbol
                                   , quantity    :: Quantity
                                   , defSynonyms :: [Symbol]} deriving (Show, Eq, Ord)


data Definitions = Definitions { bases          :: M.Map String (Double, CompositeUnit)
                               , synonyms       :: M.Map String String
                               , unitsList      :: [String]
                               , prefixes       :: [String]
                               , prefixValues   :: M.Map String Double
                               , prefixSynonyms :: M.Map String String
                               , unitTypes      :: M.Map String String } deriving (Show, Eq, Ord)

emptyDefinitions :: Definitions
emptyDefinitions = Definitions { bases          = M.empty
                               , synonyms       = M.empty
                               , unitsList      = []
                               , prefixes       = []
                               , prefixValues   = M.fromList [("", 1)]
                               , prefixSynonyms = M.fromList [("", "")]
                               , unitTypes      = M.empty }


-- | Combine two Definitions structures
unionDefinitions :: Definitions -> Definitions -> Definitions
unionDefinitions d1 d2 = Definitions {
  bases = bases d1 `M.union` bases d2
  , synonyms = synonyms d1 `M.union` synonyms d2
  , unitsList = unitsList d1 ++ unitsList d2
  , prefixes = prefixes d1 ++ prefixes d2
  , prefixValues = prefixValues d1 `M.union` prefixValues d2
  , prefixSynonyms = prefixSynonyms d1 `M.union` prefixSynonyms d2
  , unitTypes = unitTypes d1 `M.union` unitTypes d2 }
