module Data.Quantities.Definitions where

import Control.Applicative ((<$>))
import Control.Monad.State
import Data.Bits (xor)
import Data.List (foldl')
import qualified Data.Map as M
import qualified Data.Set as S

import Data.Quantities.Convert (convertBase')
import Data.Quantities.Data
import Data.Quantities.DefinitionParser (parseDefinitions)
import Data.Quantities.ExprParser (preprocessQuantity)

-- | Convert string of definitions into 'Definitions' structure. See source
-- code for 'Data.Quantities.defaultDefString' for an example.
readDefinitions :: String -> Either QuantityError Definitions
readDefinitions s = addDefinitionsHash s <$> d
  where d = makeDefinitions (parseDefinitions s)

type DefineMonad = StateT Definitions (Either QuantityError)
makeDefinitions :: [Definition] -> Either QuantityError Definitions
makeDefinitions ds = execStateT (mapM addDefinition ds) emptyDefinitions

addDefinition :: Definition -> DefineMonad ()

addDefinition (PrefixDefinition sym fac syns) = do
  d <- get
  let newd = emptyDefinitions {
        prefixes         = sym : syns
        , prefixValues   = M.singleton sym fac
        , prefixSynonyms = M.fromList $ zip (sym : syns) (repeat sym) }
      defCheck = checkDefined (sym : syns) (prefixes d)
  if null defCheck
    then put $ d `unionDefinitions` newd
    else lift . Left . PrefixAlreadyDefinedError $ head defCheck ++ "-"

addDefinition (BaseDefinition sym utype syns) = do
  d <- get
  let defCheck = checkDefined (sym : syns) (unitsList d)
  if null defCheck
    then put $ d `unionDefinitions` emptyDefinitions {
      bases         = M.singleton sym (1, [SimpleUnit sym "" 1])
      , unitsList   = sym : syns
      , synonyms    = M.fromList $ zip (sym : syns) (repeat sym)
      , unitTypes   = M.singleton sym utype }
    else lift . Left $ UnitAlreadyDefinedError (head defCheck)

addDefinition (UnitDefinition sym q syns) = do
  -- First, we preprocess the quantity so all units are base units and
  -- prefixes are preprocessed. Then we do the standard Definitions
  -- modification like prefix and base definitions.
  d <- get
  let pq = preprocessQuantity d q
      defCheck = checkDefined (sym : syns) (unitsList d)
  if null defCheck
    then case pq of
      (Right pq') -> do
        let (Quantity baseFac baseUnits _) = convertBase' d pq'
        put $ d `unionDefinitions` emptyDefinitions {
          bases         = M.singleton sym (baseFac, baseUnits)
          , synonyms    = M.fromList $ zip (sym : syns) (repeat sym)
          , unitsList   = sym : syns }
      (Left err) -> lift . Left $ err
    else lift . Left $ UnitAlreadyDefinedError $ head defCheck


-- | Computes intersection of two lists
checkDefined :: [Symbol] -> [Symbol] -> [Symbol]
checkDefined a b = S.toList $ S.intersection (S.fromList a) (S.fromList b)


-- | Variant of the DJB2 hash; <http://stackoverflow.com/a/9263004/1333514>
hash :: String -> Int
hash = foldl' (\h c -> 33*h `xor` fromEnum c) 5381

-- | Add a hash of the definitions string to a group of Definitions. Meant to
-- be the last step, after definitions are created. Used for Definitions
-- comparison.
addDefinitionsHash :: String -> Definitions -> Definitions
addDefinitionsHash s d = d { defStringHash = hash s }
