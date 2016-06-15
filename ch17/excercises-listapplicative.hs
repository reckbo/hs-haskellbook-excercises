import           Test.QuickCheck
import           Test.QuickCheck.Checkers
import           Test.QuickCheck.Classes

data List a = Nil | Cons a (List a)
  deriving (Eq, Show)

instance Functor List where
  fmap f Nil = Nil
  fmap f (Cons x xs) = Cons (f x) (fmap f xs)


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

instance Applicative List where
  pure x = Cons x Nil
  (<*>) Nil _ = Nil
  (<*>) _ Nil = Nil
  (<*>) fs xs = flatMap (\g->fmap g xs) fs

take' :: Int->List a->List a
take'  0 _ = Nil
take' _ Nil = Nil
take' n (Cons x xs) = Cons x (take' (n-1) xs)

newtype ZipList' a = ZipList' (List a)
  deriving (Eq, Show)

instance Eq a => EqProp (ZipList' a) where
  xs =-= ys = xs' `eq` ys'
    where xs' = let (ZipList' l) = xs
            in take' 3000 l
          ys' = let (ZipList' l) = ys
            in take' 3000 l

instance Functor ZipList' where
  fmap f (ZipList' xs) = ZipList' $ fmap f xs

zipWith' :: (a->b->c) -> List a -> List b -> List c
zipWith' _ Nil _ = Nil
zipWith' _ _ Nil = Nil
zipWith' f (Cons x xs) (Cons y ys) = Cons (f x y) (zipWith' f xs ys)

instance Applicative ZipList' where
  pure x = ZipList' (Cons x Nil)
  -- (<*>) (ZipList' Nil) _ = pure Nil
  -- (<*>) _ (ZipList' Nil) _ = pure Nil
  (<*>) (ZipList' fs) (ZipList' xs) = ZipList' $ zipWith' ($) fs xs

instance Arbitrary a => Arbitrary (ZipList' a) where
  arbitrary = ZipList' <$> arbitrary

instance Arbitrary a => Arbitrary (List a) where
  arbitrary = do
    a <- arbitrary
    return (Cons a Nil)

trigger = undefined :: ZipList' (String, String, Int)

main :: IO ()
main = quickBatch $ applicative trigger
