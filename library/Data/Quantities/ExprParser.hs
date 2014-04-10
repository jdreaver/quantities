-- | Parse expressions with numbers and units.
--
-- This module provides a basic expression grammar that parses numbers
-- and units.
module Data.Quantities.ExprParser  where

import Control.Applicative ((<*>), (<$>), (*>), (<*))
import Data.Either (partitionEithers)
import qualified Data.Map as M
import Numeric (readFloat)
import Text.ParserCombinators.Parsec

import Data.Quantities.Convert (addQuants, subtractQuants, convert)
import Data.Quantities.Data

-- | Alternate definition for spaces. Just actual spaces.
spaces' :: Parser String
spaces' = many $ char ' '

-- | Parse quantity expression; addition and subtraction allowed.
parseExprQuant :: Definitions -> String -> Either QuantityError Quantity
parseExprQuant d input = case parse (parseExpr d) "arithmetic" input of
  Left err  -> Left $ ParserError $ show err
  Right val -> val

-- | Simple type used for shorthand
type EQuant = Either QuantityError Quantity

-- | Using already compiled definitions, parse expression. Also allows for
-- expressions like "exp1 => exp2" in the middle, which converts the quantity
-- exp1 into the units of the quantity exp2.
parseExpr :: Definitions -> Parser EQuant
parseExpr d = do
  _ <- spaces'
  exp1 <- parseExpr' d <* spaces
  exp2 <- optionMaybe $ string "=>" >> spaces >> parseExpr' d
  _ <- spaces'
  case exp2 of
    -- No conversion. Just return the first quantity.
    Nothing      -> return exp1
    -- Convert the first quantity to the units of the second.
    (Just exp2') -> return $ do
      e1 <- exp1
      e2 <- units <$> exp2'
      convert e1 e2

parseExpr', parseTerm :: Definitions -> Parser EQuant
parseFactor, parseExpt, parseNestedExpr :: Definitions -> Parser EQuant
parseExpr'      d = try (parseTermOp d)     <|> parseTerm d
parseTerm       d = try (parseFactorOp d)   <|> parseFactor d
parseFactor     d = try (parseExptOp d)     <|> parseExpt d
parseExpt       d = try (parseNestedExpr d) <|> parseESymbolNum d
parseNestedExpr d = spaces' >> char '(' *> spaces' >>
                    parseExpr' d
                    <* spaces' <* char ')' <* spaces' <?> "parseNested"

parseExptOp, parseTermOp, parseFactorOp :: Definitions -> Parser EQuant
parseExptOp   d = parseExpt d  `chainl1` exptOp
parseTermOp   d = parseTerm d  `chainl1` addOp
parseFactorOp d = parseFactor d `chainl1` mulOp


exptOp, addOp, mulOp :: Parser (EQuant -> EQuant -> EQuant)
addOp = try parseAdd <|> parseSubtract <?> "addOp"
  where parseAdd      = char '+' >> spaces' >> return addEQuants
        parseSubtract = char '-' >> spaces' >> return subtractEQuants
mulOp = try parseTimes <|> try parseDiv <|> parseImplicitTimes <?> "mulOp"
  where parseTimes         = char '*' >> spaces' >> return multiplyEQuants
        parseDiv           = char '/' >> spaces' >> return divideEQuants
        parseImplicitTimes = return multiplyEQuants
exptOp = try (opChoice >> spaces' >> return exptEQuants) <?> "expOp"
  where opChoice = string "^" <|> string "**"

-- | Modification of addQuants to account for Either QuantityError Quantity.
addEQuants :: EQuant -> EQuant -> EQuant
addEQuants (Right a) (Right b) = addQuants a b
addEQuants (Left a) _          = Left a
addEQuants _ (Left b)          = Left b

-- | Modification of subtractQuants to account for Either QuantityError Quantity.
subtractEQuants :: EQuant -> EQuant -> EQuant
subtractEQuants (Right a) (Right b) = subtractQuants a b
subtractEQuants (Left a) _          = Left a
subtractEQuants _ (Left b)          = Left b

-- | Modification of multiplyQuants to account for Either QuantityError Quantity.
multiplyEQuants :: EQuant -> EQuant -> EQuant
multiplyEQuants (Right a) (Right b) = Right $ multiplyQuants a b
multiplyEQuants (Left a) _          = Left a
multiplyEQuants _ (Left b)          = Left b

-- | Modification of divideQuants to account for Either QuantityError Quantity.
divideEQuants :: EQuant -> EQuant -> EQuant
divideEQuants (Right a) (Right b) = Right $ divideQuants a b
divideEQuants (Left a) _          = Left a
divideEQuants _ (Left b)          = Left b

-- | Modification of exptQuants to account for Either QuantityError Quantity.
-- Returns error if dimensional quantity used in exponent.
exptEQuants :: EQuant -> EQuant -> EQuant
exptEQuants (Left a) _          = Left a
exptEQuants _ (Left b)          = Left b
exptEQuants (Right q) (Right (Quantity y (CompoundUnit _ []))) = Right $ exptQuants q y
exptEQuants a b  = Left $ ParserError $ "Used non-dimensionless exponent in " ++ showq
  where showq = unwords ["(", show a, ") ** (", show b, ")"]

-- | Modification of parseSymbolNum to handle parsing errors.
parseESymbolNum :: Definitions -> Parser EQuant
parseESymbolNum d = try (parseENum d) <|> parseESymbol d

-- | Parses a symbol and then parses a prefix form that symbol.
parseESymbol :: Definitions -> Parser EQuant
parseESymbol d = do
  q <- parseSymbol'
  return $ preprocessQuantity d q

-- | Parse a number and insert the given definitions into the CompoundUnit.
parseENum :: Definitions -> Parser EQuant
parseENum d = do
  q <- parseNum
  return $ Right $ q { units = (units q) { defs = d } }

-- | Parses out prefixes and aliases from quantity's units.
preprocessQuantity :: Definitions -> Quantity -> Either QuantityError Quantity
preprocessQuantity d (Quantity x us)
  | null errs = Right $ Quantity x (CompoundUnit d us')
  | otherwise = Left  $ head errs
    where ppUnits     = map (preprocessUnit d) (sUnits us)
          (errs, us') = partitionEithers ppUnits

-- | Parses prefix and alias, if applicable, from a SimpleUnit.
preprocessUnit :: Definitions -> SimpleUnit -> Either QuantityError SimpleUnit
preprocessUnit d (SimpleUnit s _ p)
  | rs `elem` unitsList d = Right $ SimpleUnit ns np p
  | otherwise             = Left  $ UndefinedUnitError s
  where (rp, rs) = prefixParser d s
        np       = prefixSynonyms d M.! rp
        ns       = synonyms d M.! rs

-- | Try to parse a prefix from a symbol. Otherwise, just return the symbol.
prefixParser :: Definitions -> String -> (String, String)
prefixParser d input = if input `elem` unitsList d
                          then ("", input)
                          else case parse (prefixParser' d) "arithmetic" input of
                            Left _ -> ("", input)
                            Right val -> splitAt (length val) input

-- | Helper function for prefixParser that is a Parsec parser.
prefixParser' :: Definitions -> Parser String
prefixParser' d = do
  pr <- choice $ map (try . string) (prefixes d)
  _  <- choice $ map (try . string) (unitsList d)
  return pr

-- | Converts string to a Quantity using an expression grammar parser. This
-- parser does not parser addition or subtraction, and is used for unit
-- definitions.
parseMultExpr :: Parser Quantity
parseMultExpr = spaces' >> parseMultExpr' <* spaces'

parseMultExpr', parseMultFactor, parseMultExpt, parseMultNestedExpr :: Parser Quantity
parseMultExpr'      = try parseMultFactorOp   <|> parseMultFactor
parseMultFactor     = try parseMultExptOp     <|> parseMultExpt
parseMultExpt       = try parseMultNestedExpr <|> parseSymbolNum
parseMultNestedExpr = spaces' >> char '(' *> spaces' >>
                      parseMultExpr'
                      <* spaces' <* char ')' <* spaces' <?> "parseNested"


parseMultExptOp, parseMultFactorOp :: Parser Quantity
parseMultExptOp     = parseMultExpt   `chainl1` exptMultOp
parseMultFactorOp   = parseMultFactor `chainl1` mulMultOp

exptMultOp, mulMultOp :: Parser (Quantity -> Quantity -> Quantity)
mulMultOp = try parseTimes <|> try parseDiv <|> parseImplicitTimes <?> "mulMultOp"
  where parseTimes         = char '*' >> spaces' >> return multiplyQuants
        parseDiv           = char '/' >> spaces' >> return divideQuants
        parseImplicitTimes = return multiplyQuants
exptMultOp = try (opChoice >> spaces' >> return exptMultQuants') <?> "expMultOp"
  where opChoice = string "^" <|> string "**"

exptMultQuants' :: Quantity -> Quantity -> Quantity
exptMultQuants' q (Quantity y (CompoundUnit _ [])) = exptQuants q y
exptMultQuants' a b  = error $ "Used non-dimensionless exponent in " ++ showq
  where showq = unwords ["(", show a, ") ** (", show b, ")"]

-- | Parse either a symbol or a number.
parseSymbolNum :: Parser Quantity
parseSymbolNum = try parseNum <|> parseSymbol'

-- | Parse a symbol with an optional negative sign. A symbol can contain
-- alphanumeric characters and the character '_'.
parseSymbol' :: Parser Quantity
parseSymbol' = do
  neg  <- option "" $ string "-"
  symf <- letter
  rest <- many (alphaNum <|> char '_')
  _ <- spaces'
  return $ baseQuant (timesSign neg 1) [SimpleUnit (symf : rest) "" 1]

-- | Parent function for parseNum' to parse a number.
parseNum :: Parser Quantity
parseNum = do
  num <- parseNum'
  return $ baseQuant num []

-- | Meat of number parser. Parse digits with an optional negative sign and
-- optional exponential. For example, -5.2e4.
parseNum' :: Parser Double
parseNum' = do
  neg <- option "" $ string "-"
  whole <- many1 digit
  decimal <- option "" $ (:) <$> char '.' <*> many1 digit
  exponential <- option "" parseExponential
  _ <- spaces'
  return $ timesSign neg $ fst $ head $ readFloat $ whole ++ decimal ++ exponential

-- | Parses just the exponential part of a number. For example, parses "4" from
-- "-5.2e4".
parseExponential :: Parser String
parseExponential = do
  e <- string "e"
  neg <- option "" $ string "+" <|> string "-"
  pow <- many1 digit
  return $ e ++ neg ++ pow

-- | Negate a number if the first argument is a negative sign.
timesSign :: String -> Double -> Double
timesSign sign x
  | sign == "-" = -x
  | otherwise   = x
