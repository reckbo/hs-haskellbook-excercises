import           Control.Applicative
import           Control.Monad
import           Test.QuickCheck
import           Test.QuickCheck.Checkers
import           Test.QuickCheck.Classes

data Sum a b = First a | Second b
  deriving (Eq, Show)

instance Functor (Sum a) where
  fmap f (First x) = First x
  fmap f (Second x) = Second $ f x

instance Applicative (Sum a) where
  pure = Second
  (<*>) (Second f) (Second x) = Second $ f x
  (<*>) (First x) (First _) = First x
  (<*>) (Second _) (First x) = First x
  (<*>) (First x) (Second _) = First x

instance Monad (Sum a) where
  return = pure
  (>>=) (Second x) f = f x
  (>>=) (First x) f = First x

instance (Arbitrary a, Arbitrary b) => Arbitrary (Sum a b) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    elements [First a, Second b]

instance (Eq a, Eq b) => EqProp (Sum a b) where
  (=-=) = eq

main :: IO ()
main = do
  let trigger = undefined :: Sum Int (String,Int,String)
  quickBatch $ monad trigger
