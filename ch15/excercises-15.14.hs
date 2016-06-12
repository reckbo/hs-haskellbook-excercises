import           Data.Semigroup
import           Test.QuickCheck

data Trivial = Trivial deriving (Eq, Show)

instance Semigroup Trivial where
  _ <> _ = Trivial

instance Arbitrary Trivial where
  arbitrary = return Trivial

semigroupAssoc :: (Eq m, Semigroup m) => m -> m -> m -> Bool
semigroupAssoc a b c = (a<>(b<>c))==((a<>b)<>c)

type TrivialAssoc = Trivial -> Trivial -> Trivial -> Bool

-- Identity a
newtype Identity a = Identity a deriving (Eq, Show)

instance Arbitrary a => Arbitrary (Identity a) where
  arbitrary = do
    a <- arbitrary
    return $ Identity a

instance Semigroup (Identity a) where
  (Identity x) <> (Identity _) = Identity x

type IdentityAssoc = (Identity Int) -> (Identity Int) -> (Identity Int) -> Bool

data Two a b = Two a b
  deriving (Show, Eq)

instance (Semigroup b, Semigroup a) => Semigroup (Two a b) where
  (Two y x) <> (Two y' x') = Two (y<>y') (x <> x')

instance (Arbitrary a, Arbitrary b) => Arbitrary (Two a b) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    return $ Two a b

type TwoAssoc = (Two Trivial String) -> (Two Trivial String) -> (Two Trivial String) -> Bool

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

main :: IO ()
main =
  -- quickCheck (semigroupAssoc :: TrivialAssoc)
  -- verboseCheck (semigroupAssoc :: IdentityAssoc)
  -- verboseCheck (semigroupAssoc :: TwoAssoc)
  -- verboseCheck (semigroupAssoc :: BoolConjAssoc)
  verboseCheck (semigroupAssoc :: OrAssoc)
