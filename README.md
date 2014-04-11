# Quantities

[![Build Status](https://travis-ci.org/jdreaver/quantities.svg?branch=master)](https://travis-ci.org/jdreaver/quantities)

Unit conversion and manipulation library for Haskell.

## Documentation

Check out the docs on [Hackage](http://hackage.haskell.org/package/quantities).

## Examples

We provide a string-parsing constructor to create quantities. This
constructor is built using an expression parser, so arithmetic
expressions can be used.

```
>>> fromString "25 m/s"
Right 25.0 meter / second
>>> fromString "fakeunit"
Left (UndefinedUnitError "fakeunit")
>>> fromString "ft + 12in"
Right 2.0 foot
```

fromString also supports unit conversions, by placing \"=>\" in between
two valid expressions.

```
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
  ft <- fmap units $ fromString "ft"
  convert m ft
```

```
>>> computation
Right 98.42519685039369 foot
```

## TODO

* Don't require Double for Quantity. Use any numeric magnitude.
* Add ability to create units out of order; base quantity does not
  already have to be defined, as long as it is defined in the file.
* Handle temperature.
