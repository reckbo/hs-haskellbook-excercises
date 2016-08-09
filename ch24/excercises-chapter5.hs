{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}

import           Data.ByteString   (ByteString)
import           Data.Time
import           Text.RawString.QQ
import           Text.Trifecta
import Data.Map
import Control.Applicative

testData :: ByteString
testData = [r|
-- whee a comment

# 2025-02-05
08:00 Breakfast
09:00 Sanitizing moisture collector
11:00 Exercising in high-grav gym
12:00 Lunch
13:00 Programming
17:00 Commuting home in rover
17:30 R&R
19:00 Dinner
21:00 Shower
21:15 Read
22:00 Sleep

# 2025-02-07 -- dates not nececessarily sequential
08:00 Breakfast -- should I try skippin bfast?
09:00 Bumped head, passed out
13:36 Wake up, headache
13:37 Go to medbay
13:40 Patch self up
13:45 Commute home for rest
14:15 Read
21:00 Dinner
21:15 Read
22:00 Sleep
|]

type Time = (Int,Int)
data LogEntry = LogEntry Time String
  deriving Show
data Log = Map Day [LogEntry]

skipEOL :: Parser ()
skipEOL = skipMany (char '\n')

skipWhitespace :: Parser ()
skipWhitespace = skipMany (char ' ' <|> char '\n')

skipComments :: Parser ()
skipComments = skipMany (do _ <- string "--"
                            skipMany (noneOf "\n")
                            skipEOL)

parseDate :: Parser (Maybe Day)
parseDate = do
  year <- integer
  char '-'
  month <- integer
  char '-'
  day <- integer
  return $ fromGregorianValid year (fromIntegral  month) (fromIntegral day)

parseDateLine :: Parser (Maybe Day)
parseDateLine = do
  _ <- token $ char '#'
  date <- parseDate
  skipComments
  return date

parseTime' :: Parser (Maybe Time)
parseTime' = do
  hr <- integer
  char ':'
  min <- integer
  return $ g (hr,min) where
    g (hr,min) | hr > 23 || hr < 0 = Nothing
               | min > 59 || min < 0 = Nothing
               | otherwise = Just (fromIntegral hr, fromIntegral min)

parseLogMsg :: Parser String
parseLogMsg = manyTill (noneOf "\n") (try (string "--"))

parseLogEntry :: Parser (Maybe LogEntry)
parseLogEntry = do
  time <- parseTime'
  skipMany (char ' ')
  log <- parseLogMsg
  return $ LogEntry <$> time <*> (Just log)

