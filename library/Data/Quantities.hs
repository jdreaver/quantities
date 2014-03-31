-- | This package is used to create and manipulate physical quantities, which
-- are a numerical value associated with a unit of measurement.
--
-- In this package, values with units are represented with the Quantity type.
-- Included is an expression parser and a huge list of predefined quantities
-- with which to parse strings into a Quantity datatype. Once created, a
-- quantity can be converted to different units or queried for its
-- dimensionality. A user can also operate on quantities arithmetically, and
-- doing so uses automatic unit conversion and simplifcation.


module Data.Quantities
       (
         -- * Constructors
         -- $constructors
         fromString
       , unitsFromString
       , Quantity
       , magnitude
       , units
       , CompositeUnit
       , SimpleUnit
         -- * Conversion
         -- $conversion
       , convert
       , convertBase
       , dimensionality
         -- * Quantity arithmetic
         -- $arithmetic
       , addQuants
       , subtractQuants
       , multiplyQuants
       , divideQuants
       , exptQuants
         -- * Error type
       , QuantityError(..)
       , QuantityComputation
       , comp
       ) where


import Data.Quantities.Constructors (fromString, unitsFromString)
import Data.Quantities.Convert (convert, convertBase, addQuants, subtractQuants,
                                dimensionality)
import Data.Quantities.Data

-- $constructors
--
-- Currently, one constructor is supported to create quantities: 'fromString'.
-- There is an included expression parser that can parse values and strings
-- corresponding to builtin units. To view defined unit types, look at the
-- /source code/ for 'Data.Quantities.DefaultUnits.defaultDefString'.

-- $conversion
--
-- These functions are used to convert quantities from one unit type to
-- another.


-- $arithmetic
--
-- Once created, quantities can be manipulated using the included arithmetic
-- functions.
--
-- >>> let (Right x) = fromString "m/s"
-- >>> let (Right y) = fromString "mile/hr"
-- >>> x `multiplyQuants` y
-- 1.0 meter mile / hour / second
-- >>> x `divideQuants` y
-- 1.0 hour meter / mile / second
-- >>> x `addQuants` y
-- Right 1.4470399999999999 meter / second
-- >>> x `subtractQuants` y
-- Right 0.55296 meter / second
-- >>> x `exptQuants` 1.5
-- 1.0 meter ** 1.5 / second ** 1.5
--
-- The functions 'multiplyQuants', 'divideQuants', and 'exptQuants' change
-- units, and the units of the result are reduced to simplest terms.
--
-- >>> x `divideQuants` x
-- 1.0
-- >>> fmap (multiplyQuants x) $ fromString "s"
-- Right 1.0 meter
-- >>> x `exptQuants` 0
-- 1.0

comp :: QuantityComputation Quantity
comp = do
  x <- fromString "BADUNIT"
  y <- unitsFromString "m/s"
  convert x y
