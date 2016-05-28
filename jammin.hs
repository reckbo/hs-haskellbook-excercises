module Jammin where

import Data.Function
import Data.List

data Fruit =
    Peach
    | Plum
    | Apple
    | Blackberry deriving (Eq, Ord, Show)

data JamJars =
    Jam { fruit :: Fruit ,
          canned :: Int 
          }
          deriving (Eq, Ord, Show)

row1 = Jam Plum 4
row2 = Jam Blackberry 9
row3 = Jam Apple 20
row4 = Jam Peach 4
row5 = Jam Plum 5
row6 = Jam Plum 6
allJam = [row1, row2, row3, row4, row5, row6]

numJars = sum . map canned

mostJars :: [JamJars] -> JamJars
mostJars = maximumBy (compare `on` canned)

sortJars :: [JamJars] -> [JamJars]
sortJars = sortBy (compare `on` fruit)

groupJam :: [JamJars] -> [[JamJars]]
groupJam = groupBy ((==) `on` fruit)

