-- | Top level module.
module Data.Quantities (units
                       ,magnitude
                       ,multiplyQuants
                       ,divideQuants
                       ,exptQuants
                       ,parseQuant
                       ,convert
                       ,convertBase
                       ,dimensionality
                       ,addQuants
                       ,subtractQuants
                       ,Quantity) where


import Data.Quantities.Convert (convert, convertBase, addQuants, subtractQuants,
                                dimensionality)
import Data.Quantities.Data (multiplyQuants, divideQuants, exptQuants,
                             Quantity(units, magnitude))
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
