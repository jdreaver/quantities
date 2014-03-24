-- | Top level module.
module Data.Quantities (units
                       ,magnitude
                       ,multiplyQuants
                       ,divideQuants
                       ,exptQuants
                       ,parseQuant
                       ,Quantity) where


import Data.Quantities.Data (units, magnitude, multiplyQuants, divideQuants,
                             exptQuants, Quantity)
import Data.Quantities.Definitions (preprocessQuantity)
import Data.Quantities.DefaultUnits (defaultDefinitions)
import Data.Quantities.ExprParser (parseExprQuant)

parseQuant :: String -> Either String Quantity
parseQuant s = ret
  where rawq = parseExprQuant s
        ret  = case rawq of
          (Left err) -> Left err
          (Right q)  -> Right (preprocessQuantity defaultDefinitions q)
