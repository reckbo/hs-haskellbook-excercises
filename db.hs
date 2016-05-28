import Data.Time
import Data.Function

data DatabaseItem = DbString String
                  | DbNumber Integer
                  | DbDate   UTCTime
                    deriving (Eq, Ord, Show)

theDatabase :: [DatabaseItem] 
theDatabase =
    [ DbDate (UTCTime
            (fromGregorian 1911 5 1)
        (secondsToDiffTime 34123)) 
    , DbNumber 9001
    , DbString "Hello, world!"
    , DbDate (UTCTime
                (fromGregorian 1921 5 1)
                (secondsToDiffTime 34123))
    ]

filterDbDate :: [DatabaseItem] -> [UTCTime]
filterDbDate = map (\(DbDate x) -> x) . filter f 
                where f (DbDate x) = True
                      f _  = False

filterDbNumber :: [DatabaseItem] -> [Integer]
filterDbNumber = map (\(DbNumber x)->x) . filter f 
                where f (DbNumber x) = True
                      f x = False

mostRecent :: [DatabaseItem] -> UTCTime 
mostRecent = (foldr max t0) . filterDbDate
        where t0 = (UTCTime (fromGregorian 0 1 1) (secondsToDiffTime 34123)) 

sumDb :: [DatabaseItem] -> Integer
sumDb = sum . filterDbNumber

avgDb :: [DatabaseItem] -> Double
avgDb = avg . filterDbNumber

avg :: [Integer] -> Double
avg x = (sum x) /. (length x)
        where x /. y = fromIntegral x /fromIntegral y


