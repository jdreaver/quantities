module Data.Quantities.Constructors where


import Data.Quantities.Data (Definitions, CompositeUnit, Quantity(units),
                             QuantityError)
import Data.Quantities.DefaultUnits (defaultDefinitions')
import Data.Quantities.DefinitionParser (readDefinitions)
import Data.Quantities.Definitions (makeDefinitions, preprocessQuantity)
import Data.Quantities.ExprParser (parseExprQuant)

defaultDefinitions :: Either QuantityError Definitions
defaultDefinitions = makeDefinitions $ readDefinitions defaultDefinitions'

d :: Definitions
(Right d) = defaultDefinitions

fromString :: String -> Either String Quantity
fromString s =
  case rawq of
    (Left err) -> Left err
    (Right q)  -> case preprocessQuantity d q of
      (Left err) -> Left $ show err
      (Right q') -> Right q'
  where rawq = parseExprQuant s


unitsFromString :: String -> Either String CompositeUnit
unitsFromString s =
  case q of
    (Left err) -> Left err
    (Right q') -> Right $ units q'
  where q = fromString s
