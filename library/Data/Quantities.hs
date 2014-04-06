-- | This package is used to create and manipulate physical quantities, which
-- are a numerical value associated with a unit of measurement.
--
-- In this package, values with units are represented with the Quantity type.
-- Included is an expression parser and a huge list of predefined quantities
-- with which to parse strings into a Quantity datatype. Once created, a
-- quantity can be converted to different units or queried for its
-- dimensionality. A user can also operate on quantities arithmetically, and
-- doing so uses automatic unit conversion and simplification.


module Data.Quantities
       (
         -- * Constructors
         -- $constructors
         fromString
       , unitsFromString
       , Definitions
       , Quantity
       , magnitude
       , units
       , CompoundUnit
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
         -- * Custom definitions
         -- $custom-defs
       , fromString'
       , readDefinitions
       , defaultDefString
         -- * Error type
       , QuantityError(..)
       , QuantityComputation
       ) where


import Data.Quantities.Constructors (fromString, fromString', unitsFromString)
import Data.Quantities.Convert (convert, convertBase, addQuants, subtractQuants,
                                dimensionality)
import Data.Quantities.Data
import Data.Quantities.Definitions (readDefinitions)
import Data.Quantities.DefaultUnits (defaultDefString)

-- $constructors
--
-- Currently, one constructor is supported to create quantities: 'fromString'.
-- There is an included expression parser that can parse values and strings
-- corresponding to builtin units. To view defined unit types, look at the
-- /source code/ for 'defaultDefString'.

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


-- $custom-defs
--
-- You don't have to use the default definitions provided by
-- 'defaultDefString'. Here is an example of adding a new unit called
-- @metric_foot@.
--
-- > myDefString = defaultDefString ++ "\n" ++ "metric_foot = 300mm"
-- > (Right d') = readDefinitions myDefString
-- > myFromString = fromString' d'
--
-- >>> myFromString "metric_foot"
-- Right 1.0 metric_foot
-- >>> convertBase <$> myFromString "metric_foot"
-- Right 0.3 meter
--
-- It is usually much easier to copy the source code for 'defaultDefString' and
-- add your definitions in the appropriate spot (for example, put @metric_foot@
-- next to the other unit definitions). Then, use 'fromString'' to create your
-- Quantity constructor.
--
-- NOTE: It is very important not to perform conversions on two quantities from
-- different Definitions. Most of the error checking for undefined units is
-- done when a unit is created, and not when performing conversions.
