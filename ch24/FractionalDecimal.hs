{-# LANGUAGE OverloadedStrings #-}
module Text.Fractions where

import Control.Applicative
import Data.Ratio ((%))
import Text.Trifecta

badFraction = "1/0"
alsoBad = "10"
shouldWork = "1/2"
shouldAlsoWork = "2/1"

parseFraction :: Parser Rational
parseFraction = do
  numerator <- decimal
  char '/'
  denominator <- decimal
  return (numerator % denominator)

virtuousFraction :: Parser Rational
virtuousFraction = do
  numerator <- decimal
  char '/'
  denominator <- decimal
  case denominator of
    0 -> fail "Denominator cannot be zero"
    _ -> return (numerator % denominator)

parseDecimal :: Parser Double
parseDecimal = do
  integerPart <- many $ oneOf "123456789"
  char '.'
  fractionalPart <- many $ oneOf "123456789"
  return $ rd $ integerPart++"."++fractionalPart
    where rd = read :: String -> Double

parseNum :: Parser (Either Rational Double)
parseNum = try (fmap Right parseDecimal) <|>
          try ( fmap Left virtuousFraction)

main :: IO ()
main = do
  print $ parseString parseNum mempty "234.2342"
  print $ parseString parseNum mempty "23/98"
  print $ parseString virtuousFraction mempty badFraction
  print $ parseString virtuousFraction mempty alsoBad
  print $ parseString virtuousFraction mempty shouldAlsoWork

