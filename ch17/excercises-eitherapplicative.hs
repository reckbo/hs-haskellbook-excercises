import           Control.Applicative
import           Data.Monoid              (Monoid, (<>))
import           Test.QuickCheck (Arbitrary, elements, arbitrary)
import           Test.QuickCheck.Checkers
import           Test.QuickCheck.Classes

data Sum a b =  First a | Second b
  deriving (Eq, Show)

instance Functor (Sum a) where
  fmap _ (First x) = First x
  fmap f (Second x) = Second (f x)

instance Applicative (Sum a) where
  pure = Second
  (<*>) (First a) (First _) = First a
  (<*>) (Second _) (First x) = First x
  (<*>) (First x) (Second _) = First x
  (<*>) (Second f) (Second x) = Second $ f x

data Validation e a = Error e | Success a
  deriving (Eq, Show)

-- same as Sum/Either
instance Functor (Validation e) where
  fmap _ (Error e) = Error e
  fmap f (Success a) = Success $ f a

-- This is different
instance Monoid e => Applicative (Validation e) where
  pure = Success
  (<*>) (Success f) (Success x) = Success $ f x
  (<*>) (Success _) (Error x) = Error x
  (<*>) (Error x) (Error x') = Error $ x<>x'
  (<*>) (Error x) (Success _) = Error x

-- applyIfBothSecond :: (Sum e) (a -> b) -> (Sum e) a ->(Sum e) b

-- applyMappendError :: Monoid e =>
--   (Validation e) (a -> b)
--   -> (Validation e) a
--   -> (Validation e) b

instance (Eq a, Eq b) => EqProp (Sum a b) where
  (=-=) = eq

instance (Eq a, Eq b) => EqProp (Validation a b) where
  (=-=) = eq

triggerSum = undefined :: Sum String (String,String,Int)
triggerValidation = undefined :: Validation String (String,String,Int)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Sum a b) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    elements [First a, Second b]

instance (Arbitrary a, Arbitrary b) => Arbitrary (Validation a b) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    elements [Error a, Success b]

main :: IO ()
main = do
  quickBatch $ applicative triggerSum
  quickBatch $ applicative triggerValidation
