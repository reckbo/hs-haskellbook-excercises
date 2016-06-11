import           Data.List       (sort)
import           Test.QuickCheck
import Data.Char (toUpper)

half x = x / 2
halfIdentity = (*2) . half

-- for any list you apply sort to
-- this property should hold
listOrdered :: (Ord a) => [a] -> Bool
listOrdered xs = snd $ foldr go (Nothing, True) xs
  where go _ status@(_, False) = status
        go y (Nothing, t) = (Just y, t)
        go y (Just x, t) = (Just y, x >=y)

newtype SortedList a = SortedList [a]
                     deriving Show

instance (Arbitrary a, Ord a) => Arbitrary (SortedList a) where
  arbitrary = do
    l <- arbitrary
    return $ SortedList $ sort l

sortedProperty :: (Ord a) => SortedList a -> Bool
sortedProperty (SortedList l)= listOrdered l

plusAssociative x y z = x+(y+z)==(x+y)+z
plusCommutative x y = x+y==y+x

multAssociative x y z = x*(y*z)==(x*y)*z
multCommutative x y = x*y==y*x

quotRemLaw x 0 = True
quotRemLaw x y = (quot x y)*y + (rem x y) == x
divModLaw x 0 = True
divModLaw x y = (div x y)*y + (mod x y)==x

expCommutative x y = x^y == y^x
expAssociative x y z = x^(y^z) == (x^y)^z

reverseEquals :: Eq a => [a] -> Bool
reverseEquals xs = (reverse . reverse $ xs) ==  xs

prop_dollar :: Int -> Bool
prop_dollar x = ( f$x ) == f x
                where f = (+9)

prop_compose :: Int -> Bool
prop_compose x = (f.g) x == f (g x)
                 where f = not
                       g = even

prop_concat :: [[Int]] -> Bool
prop_concat xss = foldr (++) [] xss == concat xss


prop_length :: Int -> [Char] -> Bool
prop_length n xs = length (take n xs) == n

prop_read :: (Read a,Show a,Eq a) => a -> Bool
prop_read x = (read (show x)) == x


square :: (Num a) => a -> a
square x = x * x
  
squareIdentity :: (Floating a) => a -> a
squareIdentity = square . sqrt

prop_squareIdentity x = x == squareIdentity x

twice f = f.f
fourTimes = twice . twice

capitalizeWords = map toUpper

prop_idempotence :: Eq a => (a->a) -> a -> Bool
prop_idempotence f x = (f x == twice f x) &&
          (f x == fourTimes f x)

main :: IO()
main = do
  quickCheck $ \x -> x == halfIdentity ( x :: Double )
  quickCheck (sortedProperty :: (SortedList [Int]) -> Bool)
  quickCheck (plusCommutative :: Integer -> Integer -> Bool)
  quickCheck (plusAssociative :: Integer -> Integer -> Integer -> Bool)
  quickCheck (multAssociative :: Integer -> Integer -> Integer -> Bool)
  quickCheck (multCommutative :: Integer -> Integer -> Bool)
  quickCheck (quotRemLaw :: Integer -> Integer -> Bool)
  quickCheck (divModLaw :: Integer -> Integer -> Bool)
  quickCheck  ( expAssociative :: Integer -> Integer -> Integer -> Bool)
  quickCheck  ( expCommutative :: Integer -> Integer -> Bool)
  quickCheck (reverseEquals :: [Int] -> Bool)
  quickCheck prop_dollar
  quickCheck prop_compose
  quickCheck prop_concat
  quickCheck prop_length
  quickCheck ( prop_read :: Integer -> Bool)
  quickCheck (prop_squareIdentity :: Double -> Bool)
  quickCheck (prop_idempotence capitalizeWords)
  quickCheck (prop_idempotence sort :: [Int] -> Bool)
