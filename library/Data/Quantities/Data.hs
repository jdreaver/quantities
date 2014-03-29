-- | Base module for all data structures.
module Data.Quantities.Data where

import Data.List (partition, sort)
import qualified Data.Map as M

-- | String representation of a unit. Examples: "meter", "foot"
type Symbol = String

-- | Representation of single unit. For example: millimeter^2 is
-- represented as SimpleUnit { symbol = "meter", prefix = "milli",
-- power = 2.0 }
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
                         , units     :: CompositeUnit
                         , defs      :: Definitions } deriving (Ord)


instance Show Quantity where
  show (Quantity m us _) = show m ++ " " ++ showCompUnit us

-- | Sort units but put negative units at end
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
                   | DimensionalityError CompositeUnit CompositeUnit
                   deriving (Show, Eq)


reduceUnits, reduceUnits', removeZeros :: CompositeUnit -> CompositeUnit
-- | Combines equivalent units and removes units with powers of zero.
reduceUnits  = removeZeros . reduceUnits' . sort

reduceUnits' [] = []
reduceUnits' (SimpleUnit x pr1 p1 : SimpleUnit y pr2 p2: xs)
  | (x,pr1) == (y,pr2) = SimpleUnit x pr1 (p1+p2) : reduceUnits' xs
  | otherwise = SimpleUnit x pr1 p1 : reduceUnits' (SimpleUnit y pr2 p2 : xs)
reduceUnits' (x:xs) = x : reduceUnits' xs

removeZeros [] = []
removeZeros (SimpleUnit _ _ 0.0 : xs) = removeZeros xs
removeZeros (x:xs) = x : removeZeros xs

invertUnits :: CompositeUnit -> CompositeUnit
invertUnits = map invertSimpleUnit

-- | Inverts unit by negating the power field.
invertSimpleUnit :: SimpleUnit -> SimpleUnit
invertSimpleUnit (SimpleUnit s pr p) = SimpleUnit s pr (-p)

-- | Multiplies Quantities by appending their units and multiplying
-- magnitudes.
multiplyQuants :: Quantity -> Quantity -> Quantity
multiplyQuants x y = Quantity mag newUnits (defs x)
  where mag      = magnitude x * magnitude y
        newUnits = reduceUnits (units x ++ units y)

-- | Divides Quantities by inverting the units of the second argument,
-- appending the units, and dividing the magnitudes.
divideQuants :: Quantity -> Quantity -> Quantity
divideQuants x y = Quantity mag newUnits (defs x)
  where mag      = magnitude x / magnitude y
        newUnits = reduceUnits (units x ++ invertUnits (units y))

-- | expt for Quantities.
exptQuants :: Quantity -> Double -> Quantity
exptQuants (Quantity x u d) y = Quantity (x**y) (expUnits u) d
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
