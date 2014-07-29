# Quantities

[![Build Status](https://travis-ci.org/jdreaver/quantities.svg?branch=master)](https://travis-ci.org/jdreaver/quantities)

Unit conversion and manipulation library for Haskell.

## Documentation

Check out the docs on [Hackage](http://hackage.haskell.org/package/quantities).

## Examples

We provide a string-parsing constructor to create quantities. This
constructor is built using an expression parser, so arithmetic
expressions can be used.

```haskell
>>> fromString "25 m/s"
Right 25.0 meter / second
>>> fromString "fakeunit"
Left (UndefinedUnitError "fakeunit")
>>> fromString "ft + 12in"
Right 2.0 foot
```

fromString also supports unit conversions, by placing "=>" in between
two valid expressions.

```haskell
>>> fromString "min => s"
Right 60.0 second
>>> fromString "2 ft + 6 in => ft"
Right 2.5 foot
```

The `convert` function is used for unit conversions.

```haskell
computation :: Either QuantityError Quantity
computation = do
  m  <- fromString "30 m"
  ft <- units <$> fromString "ft"
  convert m ft
```

```haskell
>>> computation
Right 98.42519685039369 foot
```

## Executable

An executable is included called `quantities`, which is an interface to the
`fromString` function.

```bash
$ quantities -h
Usage: quantities [-vh] expression
$ quantities "10 bbl/sec => m^3/min"
95.39237695680004 meter ** 3 / minute
$ quantities "fakeunit"
UndefinedUnitError "fakeunit"
```

## What units are defined?

We have already defined an extensive list of units and SI prefixes. To
view them, check out
[this source file](https://github.com/jdreaver/quantities/blob/master/library/Data/Quantities/DefaultUnits.hs).

There is also functionality to modify this file, or create a totally new one.

## How to Contribute

Head over to the [Github repo](https://github.com/jdreaver/quantities)
to report an issue or create a patch. Also, **non-code contributions
are always welcome**, especially in this early stage of development.
That includes, but is not limited to:

* API changes/suggestions
* Code style suggestions
* Unclear documentation

Don't feel shy to raise an issue! Any constructive criticism helps.


## TODO

* Handle temperature units. Simple temperature conversions are easy
  (celsius to farenheit), but compound units with temperatures are
  tougher.
* Don't require Double for Quantity. Use any numeric magnitude.
  - Might need typeclass for multiplication/exponentiation by a scalar (double)
* Add ability to define units out of order; base quantity does not
  already have to be defined, as long as it is defined in the file.
