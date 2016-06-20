import           Control.Applicative
import           Control.Monad
import           Data.Monoid
import           Test.QuickCheck
import           Test.QuickCheck.Checkers
import           Test.QuickCheck.Classes

-- 1.

data Nope a = NopeDotJpg
  deriving (Show, Eq)

instance Functor Nope where
  fmap _ _ = NopeDotJpg

instance Applicative Nope where
  pure = const NopeDotJpg
  x <*> x' = NopeDotJpg

instance Monad Nope where
  return = pure
  (>>=) NopeDotJpg f = NopeDotJpg

instance Arbitrary (Nope a) where
  arbitrary = return NopeDotJpg

instance EqProp (Nope a) where
  (=-=) = eq

triggerNope = undefined :: Nope (Int, String, Int)

-- 2.

data PhhhbbtttEither b a = Left' a | Right' b
  deriving (Eq, Show)

instance Functor (PhhhbbtttEither b) where
  fmap _ (Right' x) = Right' x
  fmap f (Left' x) = Left' $ f x

instance Applicative (PhhhbbtttEither b) where
  pure = Left'
  (<*>) (Left' f) (Left' x) = Left' $ f x
  (<*>) (Right' x) (Left' _) = Right' x
  (<*>) (Left' _) (Right' x) = Right' x
  (<*>) (Right' x) (Right' _) = Right' x

instance Monad (PhhhbbtttEither b) where
  return = pure
  (>>=) m f = join' $ fmap f m where
    join' (Right' x) = Right' x
    join' (Left' (Left' x)) = Left' x
    join' (Left' (Right' x)) = Right' x

instance (Arbitrary a, Arbitrary b) => Arbitrary (PhhhbbtttEither b a) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    elements [Left' a, Right' b]

instance (Eq a, Eq b) => EqProp (PhhhbbtttEither b a) where (=-=) = eq

triggerPhhh = undefined :: PhhhbbtttEither Int (Int, String, String)

  -- 3.

newtype Identity a = Identity a
  deriving (Eq, Ord, Show)

instance Functor Identity where
  fmap f (Identity x) = Identity $ f x

instance Applicative Identity where
  pure = Identity
  (<*>) (Identity f) (Identity x) = Identity $ f x

instance Monad Identity where
  return = pure
  (>>=) m f = join' $ fmap f m where
    join' (Identity x) = x

instance Arbitrary a => Arbitrary (Identity a) where
  arbitrary = Identity <$> arbitrary

instance (Eq a) => EqProp (Identity a) where
  (=-=) = eq

triggerIdentity = undefined :: Identity (Int, String, Int)

  -- 4.

data List a = Nil | Cons a (List a)
  deriving (Show, Eq)

instance Functor List where
  fmap f Nil = Nil
  fmap f (Cons x xs) = Cons (f x) (fmap f xs)

instance Applicative List where
  pure x = Cons x Nil
  (<*>) Nil _ = Nil
  (<*>) _ Nil = Nil
  (<*>) fs xs = flatMap (\g->fmap g xs) fs
    where

append :: List a ->List a ->List a
append Nil ys = ys
append (Cons x xs) ys = Cons x $ xs `append` ys
fold::(a->b->b)->b->List a ->b
fold _ b Nil = b
fold f b (Cons h t) = f h (fold f b t)
concat' :: List (List a) -> List a
concat' = fold append Nil
flatMap::(a->List b)->List a->List b
flatMap f = concat' . fmap f

instance Monad List where
  return = pure
  (>>=) m f = join' $ fmap f m
    where join' = concat'

instance (Arbitrary a) => Arbitrary (List a) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    c <- arbitrary
    elements [Nil, Cons a Nil, Cons b (Cons c Nil), Cons c (Cons a (Cons b Nil))]

instance (Eq a) => EqProp (List a) where
  (=-=) = eq

triggerList = undefined :: List (Int, String, String)


main :: IO ()
main = do
  quickBatch $ monad triggerNope
  quickBatch $ monad triggerPhhh
  quickBatch $ monad triggerIdentity
  quickBatch $ monad triggerList

