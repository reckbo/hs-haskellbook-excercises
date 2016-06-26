{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
import           Data.Monoid
import           Test.QuickCheck
import           Test.QuickCheck.Checkers
import           Test.QuickCheck.Classes

newtype Identity a = Identity a
  deriving (Eq, Ord, Show)

instance Functor Identity where
  fmap f (Identity x) = Identity $ f x

instance Applicative Identity where
  pure = Identity
  (<*>) (Identity f) (Identity x) = Identity $ f x

instance Foldable Identity where
  foldr f x0 (Identity x) = f x x0

instance Traversable Identity where
  traverse f (Identity x) = Identity <$> f x

instance Arbitrary a => Arbitrary (Identity a) where
  arbitrary = Identity <$> arbitrary

instance (Eq a) => EqProp (Identity a) where
  (=-=) = eq


newtype Constant a b = Constant { getConstant :: a }
  deriving (Show, Eq)

instance Functor (Constant a) where
  fmap _ (Constant x) = Constant x

instance Monoid a => Applicative (Constant a) where
  pure _ = Constant mempty
  (Constant x) <*> (Constant y) = Constant $ x<>y

instance Foldable (Constant a) where
  foldMap _ (Constant _) = mempty

instance Traversable (Constant a) where
  traverse _ (Constant x) = pure $ Constant x

instance Monoid a => Arbitrary (Constant a b) where
  arbitrary = pure (Constant mempty)

instance (Eq a, Eq b) => EqProp (Constant a b) where
  (=-=) = eq

data Optional a = Nada | Yep a
  deriving (Eq, Show)

instance Foldable Optional where
  foldr f x0 (Yep x) = f x x0
  foldr _ x0 Nada = x0

instance Functor Optional where
  fmap _ Nada = Nada
  fmap f (Yep x) = Yep $ f x

instance Traversable Optional where
  traverse f (Yep x) = Yep <$> f x
  traverse _ Nada = pure Nada

instance Arbitrary a => Arbitrary (Optional a) where
  arbitrary = do
    a <- arbitrary
    elements [Nada, Yep a]

instance Eq a => EqProp (Optional a) where
  (=-=) = eq

data List a = Nil | Cons a (List a)
  deriving (Eq, Show)

instance Functor List where
  fmap _ Nil = Nil
  fmap f (Cons x xs) = Cons (f x) (fmap f xs)

instance Foldable List where
  foldr _ x0 Nil = x0
  foldr f x0 (Cons x xs) = f x (foldr f x0 xs)

instance Traversable List where
  sequenceA Nil = pure Nil
  sequenceA (Cons x xs) = Cons <$> x <*> sequenceA xs

instance Arbitrary a => Arbitrary (List a) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    elements [Nil, (Cons a Nil), (Cons b (Cons a Nil))]

instance Eq a => EqProp (List a) where
  (=-=) = eq


data Three a b c =  Three a b c
  deriving (Eq, Show)

instance Foldable (Three a b) where
  foldr f x0 (Three _ _ x) = f x x0

instance Functor (Three a b) where
  fmap f (Three a b x) = Three a b (f x)

instance Traversable (Three a b) where
  sequenceA (Three a b x) = Three a b <$> x

instance (Arbitrary a, Arbitrary b, Arbitrary c) => Arbitrary (Three a b c) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    c <- arbitrary
    return $ Three a b c

instance (Eq a, Eq b, Eq c) => EqProp (Three a b c) where
  (=-=) = eq

data Three' a b = Three' a b b
  deriving (Eq, Show)

instance Functor (Three' a) where
  fmap f (Three' a x x') = Three' a (f x) (f x')

instance Foldable (Three' a) where
  foldMap f (Three' _ x x') = f x <> f x'

instance Traversable (Three' a) where
  sequenceA (Three' a x x') = Three' a <$> x <*> x'

instance (Arbitrary a, Arbitrary b) => Arbitrary (Three' a b) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    b' <- arbitrary
    return $ Three' a b b'

instance (Eq a, Eq b) => EqProp (Three' a b) where
  (=-=) = eq

data S n a = S (n a) a
  deriving (Eq, Show)

instance (Functor n) => Functor (S n) where
  fmap f (S na a) = S (f <$> na) (f a)

instance Foldable n => Foldable (S n) where
  foldMap f (S na a) = foldMap f na <> f a

instance Traversable n => Traversable (S n) where
  traverse f (S na a) = S <$> traverse f na <*> f a

instance (Applicative n, Arbitrary a) => Arbitrary (S n a) where
  arbitrary = do
    a <- arbitrary
    return (S (pure a) a)

instance (Eq a, Eq (n a)) => EqProp (S n a) where
  (=-=) = eq

type TI = S Maybe

main :: IO ()
main = do
  let trigger = undefined :: TI (Int, Int, [Int])
  quickBatch (traversable trigger)
