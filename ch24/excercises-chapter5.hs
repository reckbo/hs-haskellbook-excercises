{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}

import           Data.ByteString   (ByteString)
import           Data.Time (fromGregorianValid, Day)
import           Text.RawString.QQ
import           Text.Trifecta
import           Data.Map            (Map)
import qualified Data.Map            as M
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
data DayLog = DayLog Day [LogEntry]
newtype Log = Log (Map Day [LogEntry])
  deriving Show

skipWhitespace :: Parser ()
skipWhitespace = skipMany (char ' ' <|> newline)

skipComments :: Parser ()
skipComments = skipMany (comment >> skipWhitespace)

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

comment :: Parser String
comment = do
  string "--"
  manyTill anyChar newline

parseLogMsg :: Parser String
parseLogMsg = manyTill anyChar (comment <|> string "\n")

parseLogEntry :: Parser (Maybe LogEntry)
parseLogEntry = do
  time <- parseTime'
  skipMany (char ' ')
  log <- parseLogMsg
  return $ LogEntry <$> time <*> (Just log)

parseDay :: Parser (Maybe DayLog)
parseDay = do
  skipWhitespace
  skipComments
  day <- parseDateLine
  entries <- some parseLogEntry
  return $ DayLog <$> day <*> (sequenceA entries)

rollup :: DayLog
        -> Map Day [LogEntry]
        -> Map Day [LogEntry]
rollup (DayLog day entries) m = M.insert day entries m

parseLog :: Parser (Maybe Log)
parseLog = do
  days <- some parseDay
  let logmap = fmap (foldr rollup M.empty) $ sequenceA days
  return $ Log <$> logmap

