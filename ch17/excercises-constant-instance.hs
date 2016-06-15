import Control.Applicative
import Data.Monoid

newtype Constant a b = Constant { getConstant :: a } deriving (Eq, Ord, Show)

instance Functor (Constant a) where
  fmap f (Constant x) = Constant x

instance Monoid a => Applicative (Constant a) where
  pure x = Constant mempty
  (<*>) (Constant x') (Constant x) = Constant (x<>x')
