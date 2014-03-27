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
                       ,Quantity) where


import Data.Quantities.Convert (convert, convertBase, addQuants, subtractQuants,
                                dimensionality)
import Data.Quantities.Data (multiplyQuants, divideQuants, exptQuants,
                             Quantity(units, magnitude), CompositeUnit)
import Data.Quantities.Definitions (preprocessQuantity)
import Data.Quantities.DefaultUnits (defaultDefinitions)
import Data.Quantities.ExprParser (parseExprQuant)

parseQuant :: String -> Either String Quantity
parseQuant s =
  case rawq of
    (Left err) -> Left err
    (Right q)  -> case preprocessQuantity defaultDefinitions q of
      (Left err) -> Left $ show err
      (Right q') -> Right q'
  where rawq = parseExprQuant s


parseUnits :: String -> Either String CompositeUnit
parseUnits s =
  case q of
    (Left err) -> Left err
    (Right q') -> Right $ units q'
  where q = parseQuant s
