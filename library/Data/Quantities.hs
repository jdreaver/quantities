-- | Top level module.
module Data.Quantities (units
                       ,magnitude
                       ,multiplyQuants
                       ,divideQuants
                       ,exptQuants
                       ,parseQuant
                       ,parseUnits
                       ,convert
                       ,convertBase
                       ,dimensionality
                       ,addQuants
                       ,subtractQuants
                       ,CompositeUnit
                       ,Quantity
                       ,QuantityError) where


import Data.Quantities.Convert (convert, convertBase, addQuants, subtractQuants,
                                dimensionality)
import Data.Quantities.Data (multiplyQuants, divideQuants, exptQuants,
                             Quantity(units, magnitude), CompositeUnit,
                             QuantityError, Definitions)
import Data.Quantities.DefinitionParser (readDefinitions)
import Data.Quantities.Definitions (preprocessQuantity, makeDefinitions)
import Data.Quantities.DefaultUnits (defaultDefinitions')
import Data.Quantities.ExprParser (parseExprQuant)

defaultDefinitions :: Either QuantityError Definitions
defaultDefinitions = makeDefinitions $ readDefinitions defaultDefinitions'

d :: Definitions
(Right d) = defaultDefinitions

parseQuant :: String -> Either String Quantity
parseQuant s =
  case rawq of
    (Left err) -> Left err
    (Right q)  -> case preprocessQuantity d q of
      (Left err) -> Left $ show err
      (Right q') -> Right q'
  where rawq = parseExprQuant s


parseUnits :: String -> Either String CompositeUnit
parseUnits s =
  case q of
    (Left err) -> Left err
    (Right q') -> Right $ units q'
  where q = parseQuant s
