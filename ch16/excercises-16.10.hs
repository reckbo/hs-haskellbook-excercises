import           Test.QuickCheck
import           Test.QuickCheck.Function

e :: IO Integer
e = let ioi = readIO "1" :: IO Integer
        changed = fmap (read . ("123"++) . show) ioi
    in fmap (*3) changed

functorIdentity :: (Functor f, Eq (f a)) => f a -> Bool
functorIdentity f = fmap id f==f

functorCompose' :: (Eq (f c), Functor f) => f a ->Fun a b ->Fun b c -> Bool
functorCompose' x (Fun _ f) (Fun _ g) = (fmap(g.f)x)==(fmap g .fmap f $ x)

type ItoI = Fun Int Int
type ItoS = Fun Int String
type StoI = Fun String Int
type ItoChar = Fun Int Char
type StoChar = Fun String Char

-- 1.

newtype Identity a = Identity a deriving ( Show, Eq )

instance Functor Identity where
  fmap f (Identity x) = Identity $ f x

instance (Arbitrary a) => Arbitrary (Identity a) where
  arbitrary = do
    a <- arbitrary
    return $ Identity a

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

data Two a b = Two a b
  deriving (Show, Eq)

instance Functor (Two a) where
  fmap f (Two x y) = Two x (f y)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Two a b) where
  arbitrary = do
     a <- arbitrary
     b <- arbitrary
     return $ Two a b

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


main :: IO ()
main = do
 -- quickCheck (functorIdentity :: Identity Int -> Bool)
 -- quickCheck (functorCompose' :: Identity Int -> ItoI -> ItoI -> Bool)
 -- quickCheck (functorIdentity :: Pair Int -> Bool)
 -- quickCheck (functorCompose' :: Identity Int -> ItoI -> ItoI -> Bool)
 -- quickCheck (functorIdentity :: Two Int String -> Bool)
 -- quickCheck (functorCompose' :: Two Int String -> StoI -> ItoS -> Bool)
 -- quickCheck (functorIdentity :: Three Int Int String -> Bool)
 -- quickCheck (functorCompose' :: Three Int Int String -> StoI -> ItoS -> Bool)
 -- quickCheck (functorIdentity :: Three' Int String -> Bool)
 -- quickCheck (functorCompose' :: Three' Int String -> StoI -> ItoS -> Bool)
 quickCheck (functorIdentity :: Four' Int String -> Bool)
 quickCheck (functorCompose' :: Four' Int String -> StoI -> ItoS -> Bool)



