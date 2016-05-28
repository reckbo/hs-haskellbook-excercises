{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}

data Airline = PapuAir | CatapultsR'Us | TakeYourChancesUnited 
    deriving (Eq, Show)

data Price = -- (a)
    Price Integer 
    deriving (Eq, Show)


data Manufacturer = -- (c)
    Mini | Mazda | Tata 
    deriving (Eq, Show)

data Vehicle = 
    Car Manufacturer Price | Plane Airline Int
    deriving (Eq, Show)

myCar = Car Mini (Price 14000) 
urCar = Car Mazda (Price 20000) 
clownCar = Car Tata (Price 7000) 
doge = Plane PapuAir 300

isCar :: Vehicle -> Bool 
isCar (Car _ _) = True
isCar _ = False

isPlane :: Vehicle -> Bool 
isPlane (Plane _ _) = True
isPlane _ = False

areCars :: [Vehicle] -> [Bool] 
areCars = map isCar 

getManu :: Vehicle -> Manufacturer 
getManu (Car m _) = m


class TooMany a where 
    tooMany :: a -> Bool

instance TooMany Int where 
    tooMany n = n > 42

newtype Goats = Goats Int 
    deriving (Eq, Show, Num, TooMany)

instance TooMany (Int,String) where
    tooMany (x,_) = x > 42 

instance (Num a, TooMany a) => TooMany (a, a) where
    tooMany (x,x') = tooMany $x+x'


{-data GuessWhat =-}
{-Chickenbutt deriving (Eq, Show)-}
{-data Id a =-}
{-MkId a deriving (Eq, Show)-}
{-data Product a b =-}
{-Product a b deriving (Eq, Show)-}
{-data Sum a b = First a-}
{-| Second b-}
{-deriving (Eq, Show)-}
{-data RecordProduct a b = RecordProduct { pfirst :: a-}
{-, psecond :: b } deriving (Eq, Show)-}


data OperatingSystem = GnuPlusLinux | OpenBSDPlusNevermindJustBSDStill | Mac | Windows deriving (Eq, Show)
data ProgrammingLanguage = Haskell | Agda | Idris | PureScript deriving (Eq, Show)
data Programmer = Programmer { os :: OperatingSystem , lang :: ProgrammingLanguage } deriving (Eq, Show)

allOperatingSystems :: [OperatingSystem] 
allOperatingSystems = [ GnuPlusLinux , OpenBSDPlusNevermindJustBSDStill , Mac , Windows ]

allLanguages :: [ProgrammingLanguage] 
allLanguages = [Haskell, Agda, Idris, PureScript]

allProgrammers :: [Programmer] 
allProgrammers = [ Programmer os lang | os <- allOperatingSystems, lang <- allLanguages ]


data BinaryTree a = Leaf | 
                    Node (BinaryTree a) a (BinaryTree a) 
                    deriving (Eq, Ord, Show)


insert' :: Ord a => a -> BinaryTree a -> BinaryTree a 
insert' b Leaf = Node Leaf b Leaf
insert' b (Node left a right)
    | b == a = Node left a right
    | b<a = Node (insert' b left) a right 
    | b>a = Node left a (insert' b right)


mapTree :: (a -> b) -> BinaryTree a -> BinaryTree b 
mapTree _ Leaf = Leaf
mapTree f (Node left a right) = Node (mapTree f left) (f a) (mapTree f right)

testTree' :: BinaryTree Integer
testTree' = Node (Node Leaf 3 Leaf) 1 (Node Leaf 4 Leaf) 

mapExpected = Node (Node Leaf 4 Leaf) 2 (Node Leaf 5 Leaf)
-- acceptance test for mapTree
mapOkay = if mapTree (+1) testTree' == mapExpected then print "yup okay!" else error "test failed!"
