-- | Top level module.
module Data.Quantities (units
                       ,magnitude
                       ,multiplyQuants
                       ,divideQuants
                       ,exptQuants
                       ,fromString
                       ,unitsFromString
                       ,convert
                       ,convertBase
                       ,dimensionality
                       ,addQuants
                       ,subtractQuants
                       ,CompositeUnit
                       ,Quantity
                       ,QuantityError) where


import Data.Quantities.Constructors (fromString, unitsFromString)
import Data.Quantities.Convert (convert, convertBase, addQuants, subtractQuants,
                                dimensionality)
import Data.Quantities.Data (multiplyQuants, divideQuants, exptQuants,
                             Quantity(units, magnitude), CompositeUnit,
                             QuantityError)
