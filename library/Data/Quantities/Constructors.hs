module Data.Quantities.Constructors where

import Data.Quantities.Data (Definitions, CompositeUnit, Quantity(units),
                             QuantityError(..))
import Data.Quantities.DefaultUnits (defaultDefString)
import Data.Quantities.DefinitionParser (readDefinitions)
import Data.Quantities.Definitions (makeDefinitions, preprocessQuantity)
import Data.Quantities.ExprParser (parseExprQuant)

defaultDefinitions :: Either QuantityError Definitions
defaultDefinitions = makeDefinitions $ readDefinitions defaultDefString

d :: Definitions
(Right d) = defaultDefinitions

-- | Create a Quantity by parsing a string. Uses an
-- 'UndefinedUnitError' for undefined units.
--
-- >>> fromString "25 m/s"
-- Right 25.0 meter / second
-- >>> fromString "fakeunit"
-- Left (UndefinedUnitError "fakeunit")

fromString :: String -> Either QuantityError Quantity
fromString s =
  case rawq of
    (Left err) -> Left $ ParserError err
    (Right q)  -> case preprocessQuantity d q of
      (Left err) -> Left err
      (Right q') -> Right q'
  where rawq = parseExprQuant s


-- | Parse units from a string. Equivalent to @fmap units . fromString@
--
-- >>> unitsFromString "N * s"
-- Right [newton,second]
unitsFromString :: String -> Either QuantityError CompositeUnit
unitsFromString = fmap units . fromString
