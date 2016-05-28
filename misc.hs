module Learn where
import Data.List (intersperse)
import Data.Char
import Data.Function
import Data.Bool

roundTrip :: (Show a, Read b) => a -> b 
roundTrip = read . show

main = do
    print (roundTrip 4::Double) 
    print (id 4)

mysum :: (Eq a, Num a) => a -> a
mysum n 
    | n == 0 = 0
    | otherwise = n + mysum (n-1) 

mymult :: (Integral a) => a -> a -> a
mymult x y 
    | x == 0  = 0
    | x == 1  = y
    | otherwise = y + mymult (x-1) y

dividedBy :: Integral a => a -> a -> Maybe (a, a) 
dividedBy _ 0 = Nothing
dividedBy num denom = go (abs num) (abs denom) 0 (num < 0 || denom < 0)
    where go n d count isNeg
            | n < d = if isNeg then Just (-count,n) else Just (count,n)
            | otherwise = go (n - d) d (count + 1) isNeg

mc91 :: (Num a, Ord a) => a -> a
mc91 n 
    | n > 100 = n -10
    | otherwise = mc91 $ mc91 $ n+11


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

eftBool :: Bool -> Bool -> [Bool] 
eftBool True False = []
eftBool False True = [False, True]
eftBool _ _ = []

eftOrd :: Ordering -> Ordering -> [Ordering] 
eftOrd LT GT = [LT,EQ,GT]
eftOrd EQ GT = [EQ,GT]
eftOrd LT EQ = [LT,EQ]
eftOrd x y 
    | x == y = [x]
    | otherwise = []

eftInt :: Int -> Int -> [Int] 
eftInt l u = go l u []
    where go lo up acc
            | lo == up = reverse $ up:acc
            | otherwise = go (succ lo) up (lo:acc)

eftChar :: Char -> Char -> [Char] 
eftChar = undefined

myWords :: Char -> String -> [String]
myWords char s = go s []
    where go s acc
            | 0 == (length s) = reverse acc
            | otherwise = go (drop 1 $ dropWhile (/= char) s) ((takeWhile (/= char) s):acc)

myzip :: [a] -> [b] -> [(a,b)]
myzip = myzipWith (,)

myzipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
myzipWith f [] _ = []
myzipWith f _ [] = []
myzipWith f (x:xs) (y:ys) = (f x y):myzipWith f xs ys

cap :: String -> String
cap "" = ""
cap (x:xs) = (toUpper x) : cap xs

{-myOr :: [Bool] -> Bool-}
{-myOr [] = False-}
{-myOr (x:xs) = x || myOr xs-}

{-myAny :: (a -> Bool) -> [a] -> Bool -}
{-myAny f [] = False-}
{-myAny f (x:xs) = (f x) || myAny f xs-}

{-myElem :: (Eq a) => a -> [a] -> Bool-}
{-myElem x [] = False-}
{-myElem e (x:xs) -}
    {-| e == x = True-}
    {-| otherwise = myElem e xs-}

myElem2 :: (Eq a) => a -> [a] -> Bool
myElem2 e xs = any (==e) xs

{-myReverse :: [a] -> [a]-}
{-myReverse x = go x []-}
    {-where go x acc = case x of-}
                        {-[] -> acc-}
                        {-(x:xs) -> go xs (x:acc)-}

{-squish :: [[a]] -> [a]-}
{-squish [] = []-}
{-squish (x:xs) = x ++ squish xs-}

{-squishMap :: (a -> [b]) -> [a] -> [b] -}
{-squishMap f = squish . (map f)-}

squishAgain :: [[a]] -> [a]
squishAgain = squishMap id

{-myMaximumBy :: (a -> a -> Ordering) -> [a] -> a -}
{-myMaximumBy f [x] = x-}
{-myMaximumBy f (x:x':xs) = case f x x' of-}
    {-GT -> myMaximumBy f (x:xs)-}
    {-LT -> myMaximumBy f (x':xs)-}
    {-EQ -> myMaximumBy f (x':xs)-}
        
{-myMinimumBy :: (a -> a -> Ordering) -> [a] -> a -}
{-myMinimumBy f [x] = x-}
{-myMinimumBy f (x:x':xs) = case f x x' of-}
    {-GT -> myMinimumBy f (x':xs)-}
    {-LT -> myMinimumBy f (x:xs)-}
    {-EQ -> myMinimumBy f (x:xs)-}

{-myMaximum :: (Ord a) => [a] -> a -}
{-myMaximum = myMaximumBy compare-}

{-myMinimum :: (Ord a) => [a] -> a -}
{-myMinimum = myMinimumBy compare-}
 
stops  = "pbtdkg"
vowels = "aeiou"
{-combos :: String -> String -> String-}
combos = [ (w1,w2,w3) | w1<-w, w2<-w, w3<-w ]
    where w = [ [c1,v,c2] | c1<-stops,v<-vowels,c2<-stops ]

fst3 :: (a,a,a) -> a
fst3 (x,_,_) = x

seekritFunc x = (sum (map length (words x))) /. (length (words x))
                where (/.)  = (/)`on`fromIntegral

myOr :: [Bool] -> Bool 
myOr = foldr (||) False

myAny :: (a -> Bool) -> [a] -> Bool
myAny = (myOr .) . map

myElem :: Eq a => a -> [a] -> Bool
myElem = (or .) . map  . (==)

myReverse :: [a] -> [a] 
myReverse = foldl (flip (:)) []

myMap :: (a -> b) -> [a] -> [b] 
myMap f = foldr ((:).f) [] 

myFilter :: (a -> Bool) -> [a] -> [a]
myFilter f = foldr (\x -> if f x then (x:) else id) []

squish :: [[a]] -> [a] 
squish = foldr (++) []

squishMap :: (a -> [b]) -> [a] -> [b] 
{-squishMap f = foldr ((++).f) []-}
squishMap = (foldr (++) [] .) . map


myMaximumBy :: (a -> a -> Ordering) -> [a] -> a 
myMaximumBy f xs = foldl maxEl (head xs) xs
                    where maxEl x y = case f x y of
                            GT -> x
                            LT -> y
                            otherwise -> x

myMinimumBy :: (a -> a -> Ordering) -> [a] -> a 
myMinimumBy f xs = foldl maxEl (head xs) xs
                    where maxEl x y = case f x y of
                            GT -> y
                            LT -> x
                            otherwise -> x

isSubsequenceOf :: (Eq a) => [a] -> [a] -> Bool
isSubsequenceOf _ [] = False
isSubsequenceOf ss s@(_:xs') 
    | (take (length ss) s) == ss = True
    | otherwise = isSubsequenceOf ss xs'

fork :: (a->b)->(b->c->d)->(a->c)->a->d 
fork f h g x = h (f x) (g x)

capitalize (x:xs) = (toUpper x):xs

capitalizeWords :: String -> [(String, String)]
{-capitalizeWords =  (fork (map capitalize) zip id). words-}
capitalizeWords s =  zip (map capitalize ws) ws
                        where ws = words s

capitalizeWords' :: String -> [(String, String)]
capitalizeWords' x = map (\s@(x:xs) -> (toUpper x:xs, s)) $ words x

{-capitalizeParagraph :: String -> [String] -}
{-capitalizeParagraph s = cap True (words s)-}
                {-where cap isBeginning (w:ws) = -}

split [] t = [t]
split (a:l) t = if a=='.' then (t:split l []) else split l (t++[a])

