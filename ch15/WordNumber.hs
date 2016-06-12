module WordNumber where

import           Data.List (intersperse)

digitToWord :: Int -> String
digitToWord n = case n of
    1 -> "one"
    2 -> "two"
    3 -> "three"
    4 -> "four"
    _ -> "five"

digits :: Int -> [Int]
digits n = go n []
    where go n l
            | n == 0 = l
            | otherwise = go new_n (digit:l)
                where digit = mod n 10
                      new_n = (n-digit) `div` 10

wordNumber :: Int -> String
wordNumber = concat . (intersperse "-") . (map digitToWord) . digits
