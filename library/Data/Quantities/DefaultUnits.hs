module Data.Quantities.DefaultUnits where

import Data.Quantities.Data (Definitions)
import Data.Quantities.DefinitionParser (readDefinitions)
import Data.Quantities.Definitions (makeDefinitions)

defaultDefinitions :: Definitions
defaultDefinitions = makeDefinitions $ readDefinitions $ unlines [
  ""
   -- Prefixes
  ,"milli- = 1e-3 = m-"
  ,"centi- = 1e-2 = c-"
  ,"kilo-  = 1e3  = k-"

   -- Length
  ,"meter = [length] = m"
  ,"foot  = 3.280839 meter = feet = ft"

   -- Time
  ,"second = [time] = s"
  ,"minute = 60 s = min"
  ]
