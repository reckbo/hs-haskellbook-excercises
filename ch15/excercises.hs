import           Data.Monoid
import           Test.QuickCheck

data Optional a = Nada | Only a
  deriving (Eq, Show)

instance Monoid a => Monoid (Optional a) where
  mempty = Nada
  mappend Nada x = x
  mappend x Nada = x
  mappend (Only x) (Only x') = Only $ mappend x x'

monoidAssoc :: (Eq m, Monoid m) => m -> m -> m -> Bool
monoidAssoc a b c = (a<>(b<>c))==((a<>b)<>c)

monoidLeftIdentity :: (Eq m, Monoid m) => m -> Bool
monoidLeftIdentity a = (mempty <> a) == a

monoidRightIdentity :: (Eq m, Monoid m) => m -> Bool
monoidRightIdentity a = (a <> mempty) == a

newtype First' a =
  First' { getFirst' :: Optional a }
  deriving (Eq, Show)

-- instance Arbitrary a => Arbitrary (First' a) where
--   arbitrary = do
--     a <- arbitrary
--     return $ First' $ getFirst' a

instance Monoid (First' a) where
  mempty = First' Nada
  mappend (First' Nada) x = x
  mappend x (First' Nada) = x
  mappend x x' = x

firstMappend :: First' a -> First' a -> First' a
firstMappend = mappend

type FirstMappend =
     First' String
  -> First' String
  -> First' String
  -> Bool

-- main :: IO ()
-- main = do
--     quickCheck (monoidAssoc :: FirstMappend)
    -- quickCheck (monoidLeftIdentity :: First' String -> Bool)
    -- quickCheck (monoidRightIdentity :: First' String -> Bool)

