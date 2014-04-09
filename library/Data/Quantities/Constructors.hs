module Data.Quantities.Constructors where

import Data.Quantities.Data
import Data.Quantities.DefaultUnits (defaultDefString)
import Data.Quantities.Definitions (readDefinitions)
import Data.Quantities.ExprParser (parseExprQuant)

defaultDefinitions :: Either QuantityError Definitions
defaultDefinitions = readDefinitions defaultDefString

-- $setup
-- >>> import Data.Quantities

d :: Definitions
(Right d) = defaultDefinitions

-- | Create a Quantity by parsing a string. Uses an 'UndefinedUnitError' for
-- undefined units. Handles arithmetic expressions as well.
--
-- >>> fromString "25 m/s"
-- Right 25.0 meter / second
-- >>> fromString "fakeunit"
-- Left (UndefinedUnitError "fakeunit")
-- >>> fromString "ft + 12in"
-- Right 2.0 foot
--
-- Make sure not to use dimensional quantities in exponents.
--
-- >>> fromString "m ** 2"
-- Right 1.0 meter ** 2.0
-- >>> fromString "m ** (2s)"
-- Left (ParserError "Used non-dimensionless exponent in ( Right 1.0 meter ) ** ( Right 2.0 second )")
fromString :: String -> Either QuantityError Quantity
fromString = parseExprQuant d

-- | Create quantities with custom definitions.
--
-- >>> let myDefString = defaultDefString ++ "\nmy_unit = 100 s"
-- >>> let (Right d) = readDefinitions myDefString
-- >>> let myFromString = fromString' d
-- >>> myFromString "25 my_unit"
-- Right 25.0 my_unit
fromString' :: Definitions -> String -> Either QuantityError Quantity
fromString' = parseExprQuant


-- | Parse units from a string. Equivalent to @fmap units . fromString@
--
-- >>> unitsFromString "N * s"
-- Right newton second
unitsFromString :: String -> Either QuantityError CompoundUnit
unitsFromString = fmap units . fromString
