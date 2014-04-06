# Quantities

[![Build Status](https://travis-ci.org/jdreaver/quantities.svg?branch=master)](https://travis-ci.org/jdreaver/quantities)

Unit conversion and manipulation library for Haskell.

## Documentation

Check out the docs on [Hackage](http://hackage.haskell.org/package/quantities).

## TODO

* Don't require Double for Quantity. Use any numeric magnitude.
* Add conversion operator to parser: "1 m/s -> ft/s"
* Need to make sure quantites have same definition file. Use hash of
  input to check for equality.
* Add ability to create units out of order; base quantity does not
  already have to be defined, as long as it is defined in the file.
* Handle temperature.
