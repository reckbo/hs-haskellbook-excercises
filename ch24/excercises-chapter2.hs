import Text.Trifecta
import Control.Applicative
import Data.List
import Data.Maybe

parseDigit :: Parser Char
parseDigit = oneOf "0123456789"

charToInt :: Char -> Integer
charToInt c = toInteger $ fromMaybe 0 $ elemIndex c "0123456789"

base10Integer' :: Parser Integer
base10Integer' = do
  sign <- option '+' (char '-')
  d <- some parseDigit
  let num = foldr (+) 0 $ zipWith (*) (reverse $ map charToInt d) (iterate (*10) 1)
  if sign == '-' then return (-num) else return num
