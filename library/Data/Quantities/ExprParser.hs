-- | Parse expressions with numbers and units.
module Data.Quantities.ExprParser  where

import Control.Applicative ((<*>), (<$>), (*>), (<*))
import Numeric (readFloat)
import System.Environment
import Text.ParserCombinators.Parsec

import Data.Quantities.Data (SimpleUnit(..), Quantity(..), baseQuant,
                             multiplyQuants, divideQuants, expQuants)

main :: IO ()
main = do
  args <- getArgs
  putStrLn $ readExpr (head args)

readExpr :: String -> String
readExpr input = case parse parseExpr "arithmetic" input of
  Left err  -> "No match: " ++ show err
  Right val -> show val

spaces' :: Parser String
spaces' = many $ char ' '

parseExprQuant :: String -> Either String Quantity
parseExprQuant input = case parse parseExpr "arithmetic" input of
  Left err  -> Left $ "No match: " ++ show err
  Right val -> Right val

-- | Converts string to a Quantity using an expression grammar parser.
parseExpr :: Parser Quantity
parseExpr = spaces' >> parseExpr' <* spaces'

-- parseExpr', parseTerm, parseFactor, parseExp, parseNestedExpr :: Parser Quantity
-- parseExpr'      = try parseTermOp     <|> parseTerm
-- parseTerm       = try parseFactorOp   <|> parseFactor
-- parseFactor     = try parseExpOp      <|> parseExp
-- parseExp        = try parseNestedExpr <|> parseSymbol
-- parseNestedExpr = spaces' >> char '(' *> spaces' >> parseExpr' <* spaces' <* char ')' <* spaces' <?> "parseNested"

-- parseExpOp, parseTermOp, parseFactorOp :: Parser Quantity
-- parseExpOp    = parseExp    `chainl1` expOp
-- parseTermOp   = parseTerm   `chainl1` addOp
-- parseFactorOp = parseFactor `chainl1` mulOp

parseExpr', parseFactor, parseExp, parseNestedExpr :: Parser Quantity
parseExpr'      = try parseFactorOp   <|> parseFactor
parseFactor     = try parseExpOp      <|> parseExp
parseExp        = try parseNestedExpr <|> parseSymbol
parseNestedExpr = spaces' >> char '(' *> spaces' >> parseExpr' <* spaces' <* char ')' <* spaces' <?> "parseNested"

parseExpOp, parseFactorOp :: Parser Quantity
parseExpOp    = parseExp    `chainl1` expOp
parseFactorOp = parseFactor `chainl1` mulOp


expOp, mulOp :: Parser (Quantity -> Quantity -> Quantity)
-- addOp = try parseAdd <|> parseSubtract <?> "addOp"
--   where parseAdd      = char '+' >> spaces' >> return (addQuants)
--         parseSubtract = char '-' >> spaces' >> return (subtractQuants)
mulOp = try parseTimes <|> try parseDiv <|> parseImplicitTimes <?> "mulOp"
  where parseTimes         = char '*' >> spaces' >> return multiplyQuants
        parseDiv           = char '/' >> spaces' >> return divideQuants
        parseImplicitTimes = return multiplyQuants
expOp = try (opChoice >> spaces' >> return expQuants) <?> "expOp"
  where opChoice = string "^" <|> string "**"

parseSymbol :: Parser Quantity
parseSymbol = try parseNum <|> parseSymbol'

parseSymbol' :: Parser Quantity
parseSymbol' = do
  neg <- option "" $ string "-"
  sym <- many1 letter
  _ <- spaces'
  return $ baseQuant (timesSign neg 1) [SimpleUnit sym "" 1]

parseNum :: Parser Quantity
parseNum = do
  num <- parseNum'
  return $ baseQuant num []

parseNum' :: Parser Double
parseNum' = do
  neg <- option "" $ string "-"
  whole <- many1 digit
  decimal <- option "" $ (:) <$> char '.' <*> many1 digit
  exponential <- option "" parseExponential
  _ <- spaces'
  return $ timesSign neg $ fst $ head $ readFloat $ whole ++ decimal ++ exponential

parseExponential :: Parser String
parseExponential = do
  e <- string "e"
  neg <- option "" $ string "-"
  pow <- many1 digit
  return $ e ++ neg ++ pow

timesSign :: String -> Double -> Double
timesSign sign x
  | sign == "-" = -x
  | otherwise   = x
