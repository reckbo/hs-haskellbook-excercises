import           Data.Monoid
import           Test.QuickCheck
import           Test.QuickCheck.Checkers
import           Test.QuickCheck.Classes
import           Test.QuickCheck.Function


-- 1.

newtype Identity a = Identity a deriving ( Show, Eq )

instance Functor Identity where
  fmap f (Identity x) = Identity $ f x

instance (Arbitrary a) => Arbitrary (Identity a) where
  arbitrary = do
    a <- arbitrary
    return $ Identity a

instance Applicative (Identity) where
  pure = Identity
  (<*>) (Identity f) (Identity x) = Identity $ f x

instance (Eq a) => EqProp (Identity a) where
  (=-=) = eq

triggerIdentity = undefined :: Identity (String, String, Int)

-- 2.

data Pair a = Pair a a
  deriving (Show, Eq)

instance Functor Pair where
  fmap f (Pair x x') = Pair (f x) (f x')

instance (Arbitrary a) => Arbitrary (Pair a) where
  arbitrary = do
    a <- arbitrary
    a' <- arbitrary
    return $ Pair a a'

instance Applicative Pair where
  pure x =  Pair x x
  (<*>) (Pair f g) (Pair x x') = Pair (f x) (g x')

instance Eq a => EqProp (Pair a) where
  (=-=) = eq

triggerPair = undefined :: Pair (String, Int, String)

-- 3.

data Two a b = Two a b
  deriving (Show, Eq)

instance Functor (Two a) where
  fmap f (Two x y) = Two x (f y)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Two a b) where
  arbitrary = do
     a <- arbitrary
     b <- arbitrary
     return $ Two a b

instance Monoid a => Applicative (Two a) where
  pure x = Two mempty x
  (<*>) (Two a f) (Two a' x) = (Two (a<>a') (f x))

instance (Eq a, Eq b) => EqProp (Two a b) where
  (=-=) = eq

triggerTwo = undefined :: Two String (String,String,Int)

-- 3.

data Three a b c = Three a b c deriving (Show, Eq)

instance Functor (Three a b) where
  fmap f (Three x y z) = Three x y (f z)

instance (Arbitrary a, Arbitrary b, Arbitrary c) => Arbitrary (Three a b c) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    c <- arbitrary
    return $ Three a b c

instance (Monoid a, Monoid b) => Applicative (Three a b) where
  pure x = Three mempty mempty x
  (<*>) (Three a b f) (Three a' b' x) = Three (a<>a') (b<>b') (f x)

instance (Eq a, Eq b, Eq c) => EqProp (Three a b c) where
  (=-=) = eq

triggerThree = undefined :: Three String String (Int, Int, String)

-- 4.

data Three' a b = Three' a b b
  deriving (Show, Eq)

instance Functor (Three' a) where
  fmap f (Three' y x x') = Three' y (f x) (f x')

instance (Arbitrary a, Arbitrary b) => Arbitrary (Three' a b) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    c <- arbitrary
    return $ Three' a b c

instance Monoid a => Applicative (Three' a) where
  pure x = Three' mempty x x
  Three' a f g <*> Three' a' x x' = Three' (a<>a') (f x) (g x')

instance (Eq a, Eq b) => EqProp (Three' a b) where
  (=-=) = eq

triggerThree' = undefined :: Three' String (String,Int,String)

-- 7.

data Four' a b = Four' a a a b
  deriving (Show, Eq)

instance Functor (Four' a) where
  fmap f (Four' y y' y'' x) = Four' y y' y'' (f x)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Four' a b) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    c <- arbitrary
    d <- arbitrary
    return $ Four' a b c d

instance (Monoid a) => Applicative (Four' a) where
  pure x = Four' mempty mempty mempty x
  Four' a aa aaa f <*> Four' a' aa' aaa' x = Four' (a<>a') (aa<>aa') (aaa<>aaa') (f x)

instance (Eq a, Eq b) => EqProp (Four' a b) where
  (=-=) = eq

triggerFour' = undefined :: Four' String (Int, Int, String)

main :: IO ()
main = do
  quickBatch $ applicative triggerIdentity
  quickBatch $ applicative triggerPair
  quickBatch $ applicative triggerTwo
  quickBatch $ applicative triggerThree
  quickBatch $ applicative triggerThree'
  quickBatch $ applicative triggerFour'
