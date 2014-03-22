-- | Base module for all data structures.
module Data.Quantities.Data where

import Data.List (sort)
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
    | p > 0     = pr ++ s ++ " ** " ++ show p
    | otherwise = "/ " ++ pr ++ s ++ " ** " ++ show (-p)

-- | Collection of SimpleUnits. Represents combination of simple
-- units.
type CompositeUnit = [SimpleUnit]

-- | Combination of magnitude and units.
data Quantity = Quantity { magnitude' :: Double
                         , units'     :: CompositeUnit
                         , defs       :: Definitions } deriving (Ord)

magnitude :: Quantity -> Double
magnitude = magnitude'

units :: Quantity -> CompositeUnit
units = units'

-- | Quantity without definitions.
baseQuant :: Double -> CompositeUnit -> Quantity
baseQuant m u = Quantity m u emptyDefinitions


-- Need to implement / and * between units. Also need custom sort to
-- put positive units in front of negative ones.
instance Show Quantity where
  show (Quantity m us _) = unwords $ show m : map show us

instance Eq Quantity where
  (Quantity m1 u1 _) == (Quantity m2 u2 _) = m1 == m2 && sort u1 == sort u2


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

-- | expt for Quantities. Can only use with nondimensional exponent.
expQuants :: Quantity -> Quantity -> Quantity
expQuants (Quantity x u d) (Quantity y [] _) = Quantity (x**y) (expUnits u) d
  where expUnits = map (\(SimpleUnit s pr p) -> SimpleUnit s pr (p*y))
expQuants _ _ = error "Used dimensional quantity as exponent."


data Definition = PrefixDefinition { defPrefix   :: Symbol
                                   , factor      :: Double
                                   , defSynonyms :: [Symbol]}
                | BaseDefinition   { base        :: Symbol
                                   , dimBase     :: Symbol
                                   , defSynonyms ::[Symbol]}
                | UnitDefinition   { defSymbol   :: Symbol
                                   , quantity    :: Quantity
                                   , defSynonyms :: [Symbol]} deriving (Show, Eq, Ord)


data Definitions = Definitions { bases          :: M.Map String String
                               , conversions    :: M.Map (String, String) Double
                               , synonyms       :: M.Map String String
                               , unitsList      :: [String]
                               , prefixes       :: [String]
                               , prefixValues   :: M.Map String Double
                               , prefixSynonyms :: M.Map String String
                               , unitTypes      :: M.Map String String } deriving (Show, Eq, Ord)

emptyDefinitions :: Definitions
emptyDefinitions = Definitions { bases          = M.empty
                               , conversions    = M.empty
                               , synonyms       = M.empty
                               , unitsList      = []
                               , prefixes       = []
                               , prefixValues   = M.fromList [("", 1)]
                               , prefixSynonyms = M.fromList [("", "")]
                               , unitTypes      = M.empty }


-- defaultDefinitions :: Definitions
-- defaultDefinitions = Definitions {
--   bases          = M.fromList [("feet", "meter")
--                               ,("meter", "meter")
--                               ,("second", "second")
--                               ,("minute", "second")]
--   , conversions    = M.fromList [(("feet", "meter"), 0.3048)
--                                 ,(("meter", "meter"), 1)
--                                 ,(("meter", "feet"), 3.280839)
--                                 ,(("second", "minute"), 1/60)
--                                 ,(("second", "second"), 1)
--                                 ,(("minute", "second"), 60)]
--   , synonyms    = M.fromList [("ft", "feet")
--                              ,("feet", "feet")
--                              ,("meter", "meter")
--                              ,("m", "meter")
--                              ,("s", "second")
--                              ,("second", "second")
--                              ,("min", "minute")
--                              ,("minute", "minute")]
--   , unitsList      = ["ft", "feet", "m", "meter", "s", "min"]
--   , prefixes       = ["milli", "m", "centi", "c", "kilo", "k"]
--   , prefixValues   = M.fromList [("milli", 1e-3)
--                                 ,("centi", 1e-2)
--                                 ,("kilo",  1e3)
--                                 ,("", 1.0)]
--   , prefixSynonyms = M.fromList [("m", "milli")
--                                 ,("milli", "milli")
--                                 ,("c", "centi")
--                                 ,("centi", "centi")
--                                 ,("k", "kilo")
--                                 ,("kilo", "kilo")
--                                 ,("", "")]
--   , unitTypes      = M.fromList [("m", "[length]")
--                                 ,("s", "[time]")]}
