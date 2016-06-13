{-# LANGUAGE DeriveGeneric        #-}
import           Data.Monoid     (Monoid, Sum, mappend, mempty)
import           Data.Semigroup  (Semigroup, (<>))
import           GHC.Generics
import           Test.QuickCheck (Arbitrary, arbitrary, elements, quickCheck,
                                  verboseCheck)

semigroupAssoc :: (Eq m, Semigroup m) => m -> m -> m -> Bool
semigroupAssoc a b c = (a<>(b<>c))==((a<>b)<>c)

-- Trivial

data Trivial = Trivial deriving (Eq, Show)

instance Semigroup Trivial where
  _ <> _ = Trivial

instance Arbitrary Trivial where
  arbitrary = return Trivial

type TrivialAssoc = Trivial -> Trivial -> Trivial -> Bool

-- Identity a
newtype Identity a = Identity a deriving (Eq, Show)

instance Arbitrary a => Arbitrary (Identity a) where
  arbitrary = do
    a <- arbitrary
    return $ Identity a

instance (Semigroup a) => Semigroup (Identity a) where
  (Identity x) <> (Identity x') = Identity $ x<>x'

type IdentityAssoc = (Identity String) -> (Identity String) -> (Identity String) -> Bool

data Two a b = Two a b
  deriving (Show, Eq)

instance (Semigroup b, Semigroup a) => Semigroup (Two a b) where
  (Two y x) <> (Two y' x') = Two (y<>y') (x <> x')

instance (Arbitrary a, Arbitrary b) => Arbitrary (Two a b) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    return $ Two a b

type TwoAssoc = (Two String String) -> (Two String String) -> (Two String String) -> Bool

-- Three a b c

data Three a b c = Three a b c deriving (Show, Eq)

instance (Semigroup a, Semigroup b, Semigroup c) => Semigroup (Three a b c) where
  (Three x y z) <> (Three x' y' z') = Three (x<>x') (y<>y') (z<>z')

instance (Arbitrary a, Arbitrary b, Arbitrary c) => Arbitrary (Three a b c) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    c <- arbitrary
    return $ Three a b c

type ThreeAssoc = (Three String String String) -> (Three String String String) -> (Three String String String) -> Bool

-- BoolConj

newtype BoolConj = BoolConj Bool
  deriving (Show, Eq)

instance Semigroup BoolConj where
  (BoolConj x) <> (BoolConj x') =  BoolConj $ x && x'

instance Arbitrary BoolConj where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    return $ BoolConj $ a && b

type BoolConjAssoc = BoolConj -> BoolConj -> BoolConj -> Bool

-- BoolDisj

newtype BoolDisj = BoolDisj Bool
  deriving (Show, Eq)

instance Semigroup BoolDisj where
  (BoolDisj x) <> (BoolDisj x') =  BoolDisj $ x || x'

instance Arbitrary BoolDisj where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    return $ BoolDisj $ a || b

type BoolDisjAssoc = BoolDisj -> BoolDisj -> BoolDisj -> Bool


-- 8. Or

data Or a b = Fst a | Snd b deriving (Show, Eq)

instance Semigroup (Or a b) where
  (<>) (Fst _) (Fst x) = Fst x
  (<>) (Fst _) (Snd x) = Snd x
  (<>) (Snd x) (Snd _) = Snd x
  (<>) (Snd x) (Fst _)  = Snd x

instance (Arbitrary a, Arbitrary b) => Arbitrary (Or a b) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    elements [Fst a, Snd b]

type OrAssoc = (Or Int String) -> (Or Int String) -> (Or Int String) -> Bool

-- 9. Combine

newtype Combine a b = Combine { unCombine :: (a -> b) }
  deriving Generic

instance Show (Combine a b) where
  show _ = "F"

instance (Semigroup b) => Semigroup (Combine a b) where
  f <> g = Combine h where
              h x = (unCombine f) x <> (unCombine g) x

instance Arbitrary (Combine a b)

type CombineAssoc = (Combine Int String) -> (Combine Int String) -> (Combine Int String) -> Bool

-- 10. Comp

newtype Comp a = Comp{unComp::(a->a)}

instance Semigroup (Comp a) where
  x <> y =  Comp $ unComp x . unComp y

type CompAssoc = (Comp String) -> (Comp String) -> (Comp String) -> Bool

-- 11.

data Validation a b = Failure a | Success b
  deriving (Eq, Show)

instance (Semigroup a) => Semigroup (Validation a b) where
  (<>) (Failure x) (Failure x') = Failure $ x<>x'
  (<>) (Success _) (Failure x) = Failure x
  (<>) (Failure x) (Success _) = Failure x
  (<>) (Success x) (Success x') = Success x

instance (Arbitrary a, Arbitrary b) => Arbitrary (Validation a b) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    elements [Failure a, Success b]

type ValidationAssoc = Validation String String -> Validation String String -> Validation String String -> Bool

-- 12.
newtype AccumulateRight a b = AccumulateRight (Validation a b)
  deriving (Eq, Show)

instance Semigroup b => Semigroup (AccumulateRight a b) where
  (<>) (AccumulateRight (Success x)) (AccumulateRight (Success x'))= AccumulateRight $ Success $ x<>x'
  (<>) (AccumulateRight (Success x)) (AccumulateRight (Failure _))= AccumulateRight $ Success x
  (<>) (AccumulateRight (Failure _)) (AccumulateRight (Success x))= AccumulateRight $ Success x
  (<>) (AccumulateRight _) (AccumulateRight y) = AccumulateRight y

instance (Arbitrary a, Arbitrary b) => Arbitrary (AccumulateRight a b) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    elements [AccumulateRight (Success b), AccumulateRight (Failure a)]

type AccumulateRightAssoc = AccumulateRight String String ->  AccumulateRight String String ->  AccumulateRight String String -> Bool

-- 13.

newtype AccumulateBoth a b = AccumulateBoth (Validation a b) deriving (Eq, Show)

instance (Semigroup a, Semigroup b) => Semigroup (AccumulateBoth a b) where
  (<>) (AccumulateBoth (Success x)) (AccumulateBoth (Success x'))= AccumulateBoth $ Success $ x<>x'
  (<>) (AccumulateBoth (Success x)) (AccumulateBoth (Failure _))= AccumulateBoth $ Success x
  (<>) (AccumulateBoth (Failure _)) (AccumulateBoth (Success x))= AccumulateBoth $ Success x
  (<>) (AccumulateBoth (Failure x)) (AccumulateBoth (Failure x')) = AccumulateBoth $ Failure $ x<>x'

instance (Arbitrary a, Arbitrary b) => Arbitrary (AccumulateBoth a b) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    elements [AccumulateBoth (Success b), AccumulateBoth (Failure a)]

type AccumulateBothAssoc = AccumulateBoth String String ->  AccumulateBoth String String ->  AccumulateBoth String String -> Bool

-- Monoids

monoidLeftIdentity :: (Monoid m, Semigroup m, Eq m) => m -> Bool
monoidLeftIdentity x = x == mempty <> x

monoidRightIdentity :: (Monoid m, Semigroup m, Eq m) => m -> Bool
monoidRightIdentity x = x == x <> mempty

-- 1.

instance Monoid Trivial where
  mempty = Trivial
  mappend = (<>)

-- 2. Identity

instance (Semigroup a, Monoid a) => Monoid (Identity a) where
  mempty = Identity mempty
  mappend = (<>)

-- 3. Two

instance (Semigroup a, Monoid a, Semigroup b, Monoid b) => Monoid (Two a b) where
  mempty = Two mempty mempty
  mappend = (<>)

-- 4. BoolConj

instance Monoid BoolConj  where
  mempty = BoolConj True
  mappend = (<>)

-- 5.

instance Monoid BoolDisj  where
  mempty = BoolDisj False
  mappend = (<>)

-- 6.

instance (Semigroup b, Monoid b) => Monoid (Combine a b) where
  mempty = Combine $ const mempty
  mappend = (<>)

-- 7.

instance Monoid (Comp a) where
  mempty = Comp id
  mappend = (<>)

-- 8.

newtype Mem s a = Mem { runMem :: s -> (a,s) }

instance Monoid a => Monoid (Mem s a) where
  mempty = Mem $ \x -> (mempty,x)
  mappend (Mem f) (Mem g) = Mem $ \x ->
                                    let (ga, gs) = g x
                                        (fa, fs) = f gs
                                    in (fa `mappend` ga, fs)

main :: IO ()
main = do
  -- quickCheck (semigroupAssoc :: TrivialAssoc)
  -- verboseCheck (semigroupAssoc :: IdentityAssoc)
  -- verboseCheck (semigroupAssoc :: TwoAssoc)
  -- quickCheck (monoidLeftIdentity :: Two String String -> Bool)
  -- quickCheck (monoidRightIdentity :: Two String String -> Bool)
  -- verboseCheck (semigroupAssoc :: BoolConjAssoc)
  -- quickCheck (monoidLeftIdentity :: BoolConj -> Bool)
  -- quickCheck (monoidRightIdentity :: BoolConj -> Bool)
  verboseCheck (semigroupAssoc :: BoolDisjAssoc)
  quickCheck (monoidLeftIdentity :: BoolDisj -> Bool)
  quickCheck (monoidRightIdentity :: BoolDisj -> Bool)
  -- verboseCheck (semigroupAssoc :: ValidationAssoc)
  -- verboseCheck ((semigroupAssoc :: AccumulateRightAssoc))
  -- verboseCheck (semigroupAssoc :: AccumulateBothAssoc)
  -- quickCheck (semigroupAssoc :: TrivialAssoc)
  -- quickCheck (monoidLeftIdentity :: Trivial -> Bool)

f' :: Mem Int String
f' = Mem $ \s->("hi",s+1)

testMem :: IO ()
testMem = do
  print $ runMem (f' `mappend` mempty) 0
  print $ runMem (mempty `mappend` f') 0
  print $ (runMem mempty 0 :: (String, Int))
  print $ runMem (f' `mappend` mempty) 0 == runMem f' 0
  print $ runMem (mempty `mappend` f') 0 == runMem f' 0

