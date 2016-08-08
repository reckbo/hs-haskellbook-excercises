{-# LANGUAGE QuasiQuotes #-}
module AltParsing where
import Control.Applicative
import Text.Trifecta
import Text.RawString.QQ

eitherOr :: String
eitherOr = [r|
123
abc
456
def
|]

type NumberOrString = Either Integer String

parseNos :: Parser NumberOrString
parseNos = skipMany (oneOf "\n") >>
  (Left <$> integer) <|> (Right <$> some letter)

main = do
  print $ parseString (some (token parseNos)) mempty eitherOr

