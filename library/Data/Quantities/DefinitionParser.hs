module Data.Quantities.DefinitionParser where

import Control.Applicative ((<$>), (<*))
import System.Environment
import Text.ParserCombinators.Parsec

import Data.Quantities.Data (Definition (..), Symbol)
import Data.Quantities.ExprParser (parseExpr, parseNum')


main :: IO ()
main = do
  defs <- readDefinitions <$> head <$> getArgs
  print defs

-- | Parse multiline string of definitions (say, from a file) into a
-- list of definitions.
readDefinitions :: String -> [Definition]
readDefinitions input = case parse readDefinitions' "Input File Parser" input of
  Left err -> error (show err) >> []
  Right val -> val

readDefinitions' :: Parser [Definition]
readDefinitions' = many parseDef <* eof

-- | Parse any definition line
parseDef :: Parser Definition
parseDef  = do
  _ <- spaces
  -- skipMany comment
  optional $ many $ char '\n'
  line <- try parseDefLine <|> try parseBaseLine <|> parsePrefixLine
  spaces
  -- skipMany comment
  optional $ many $ char '\n'
  return line

-- comment :: Parser String
-- comment = char '#' >> many (noneOf "\n")

eol :: Parser Char
eol = char '\n'

-- | Parse a line containing unit definition
-- Ex: minute = 60 s = min
parseDefLine :: Parser Definition
parseDefLine = do
  (UnitDefinition s e []) <- parseUnitDef
  syns <- many (try parseSynonym)
  return $ UnitDefinition s e syns

parseUnitDef :: Parser Definition
parseUnitDef = do
  sym   <- parseSymbol <* spaces <* char '='
  quant <- parseExpr
  spaces
  -- skipMany comment
  return $ UnitDefinition sym quant []

parseSynonym :: Parser Symbol
parseSynonym = spaces >> char '=' >> spaces >> parseSymbol <* spaces


-- | Parse line containing base definition
-- Ex: meter = [length] = m
parseBaseLine :: Parser Definition
parseBaseLine = do
  (sym, f) <- parseBase
  syns <- many (try parseSynonym)
  return $ BaseDefinition sym f syns

parseBase :: Parser (Symbol, Symbol)
parseBase = do
  sym <- parseSymbol <* spaces <* char '='
  b <- spaces >> char '[' >> parseSymbol <* char ']'
  spaces
  -- skipMany comment
  return (sym, b)

-- | Parse line containing prefix definition
-- Ex: milli- = 1e-3 = m-
parsePrefixLine :: Parser Definition
parsePrefixLine = do
  (p, f) <- parsePrefix
  syns <- many (try parsePrefixSynonym)
  return $ PrefixDefinition p f syns

parsePrefix :: Parser (Symbol, Double)
parsePrefix = do
  pre <- parseSymbol <* char '-' <* spaces <* char '='
  fac <- spaces >> parseNum'
  spaces
  -- skipMany comment
  return (pre, fac)

parsePrefixSynonym :: Parser Symbol
parsePrefixSynonym = spaces >> char '=' >> spaces >> parseSymbol <* char '-' <* spaces


-- | Parse a symbol for a unit
parseSymbol :: Parser Symbol
parseSymbol = many1 (letter <|> char '_')
