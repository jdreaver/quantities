-- | Top level module.
module Data.Quantities (units
                       ,magnitude
                       ,multiplyQuants
                       ,divideQuants
                       ,exptQuants
                       ,qString) where

import Data.Quantities.Data
import Data.Quantities.Definitions
import Data.Quantities.DefaultUnits
import Data.Quantities.ExprParser

qString :: String -> Either String Quantity
qString s = ret
  where rawq = parseExprQuant s
        ret = case rawq of
          (Left err) -> Left err
          (Right q)  -> Right $ preprocessQuantity defaultDefinitions q
