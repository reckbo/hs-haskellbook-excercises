import           Control.Applicative
import           Data.Monoid

newtype Constant a b = Constant a
  deriving (Eq, Show)

instance Foldable (Constant a) where
  foldr _ x0 _ = x0


data Two a b = Two a b
  deriving (Eq, Show)

instance Foldable (Two a) where
  foldr f x0 (Two x x') = f x' x0


data Three a b c = Three a b c deriving Show

instance Foldable (Three a b) where
  foldr f x0 (Three a b x) =  f x x0


data Three' a b = Three' a b b deriving Show

instance Foldable (Three' a) where
  foldr f x0 (Three' a b x) = f x x0

data Four' a b = Four' a b b b deriving Show

instance Foldable (Four' a) where
  foldr f x0 (Four' _ _ _ x) = f x x0


filterF :: (Applicative f, Foldable t, Monoid (f a)) => (a->Bool)-> t a -> f a
filterF g xs = foldMap (\x -> if g x then pure x else mempty) xs
