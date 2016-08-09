{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}
import Text.Trifecta
import Control.Applicative

type NumberingPlanArea = Int -- aka area code
type Exchange = Int

type LineNumber = Int

data PhoneNumber =
  PhoneNumber NumberingPlanArea Exchange LineNumber
  deriving (Eq, Show)

skipInternational :: Parser ()
skipInternational = skipOptional $ try (digit >> char '-')

parseArea :: Parser Int
parseArea = do
  let num = count 3 digit
  parsed <- num <|> between (char '(') (char ')') num
  return $ read parsed

skipSep :: Parser ()
skipSep = do
  skipOptional $ try (char ' ')
  skipOptional $ try (char '-')

parseExchange :: Parser Int
parseExchange = do
  skipSep
  fmap read $ count 3 digit

parseLineNo :: Parser Int
parseLineNo = do
  skipSep
  fmap read $ count 4 digit


parsePhone :: Parser PhoneNumber
parsePhone = do
    skipInternational
    area <- parseArea
    exchange <- parseExchange
    lineno <- parseLineNo
    return $ PhoneNumber area exchange lineno

main :: IO ()
main = do
 print $ parseString parsePhone mempty "123-456-7890"
 print $ parseString parsePhone mempty "1234567890"
 print $ parseString parsePhone mempty "(123) 456-7890"
 print $ parseString parsePhone mempty "1-123-456-7890"
