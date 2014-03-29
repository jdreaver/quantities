# Quantities

[![Build Status](https://travis-ci.org/jdreaver/quantities.svg?branch=master)](https://travis-ci.org/jdreaver/quantities)

Unit conversion and manipulation library for Haskell.

## TODO

* Add ability to create units out of order; base quantity does not
  already have to be defined, as long as it is defined in the file.
* Add line numbers to definition parser
* Check that synonyms aren't already defined too
* Handle temperature!
* Write a test that groups units by base (length, volume, 1/time, etc)
  and does an n^2 conversion between all units in the groups.
* Create unit computation monad so we don't check Either's for every function.