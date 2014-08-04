quantities changelog
====================

0.4 (unreleased)
----------------

- Added an executable that is an interface to `fromString`
- Can use " per " as a synonym for "=>" in `fromString`


0.3 (4-15-2014)
----------------

- Added unit conversion to fromString using "x => y"
- Made dimensionality printing nicer.
- Added doctests and doc coverage tests.


0.2 (4-6-2014)
--------------

- Can handle arithmetical expressions in fromString
- Added support for user-defined definitions
- Added check that definitions are the same for conversions


0.1 (3-31-2014)
---------------

- Full implementation of multiplicative dimensional quantities (no offsets yet
  for temperatures).
- Support for conversions and monadic quantity computations.
- Builtin expression parser.
- Simple definitions file format, fully stocked with units.
- Travis CI, HSpec units tests, and HLint check.
