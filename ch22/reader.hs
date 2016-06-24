{-# LANGUAGE InstanceSigs #-}
newtype Reader r a = Reader { runReader :: r -> a }

instance Functor (Reader r) where
  fmap f (Reader ra) = Reader $ (f . ra)

myLiftA2 :: Applicative f => (a->b->c) -> f a -> f b -> f c
myLiftA2 g fa fb = g <$> fa <*> fb

asks::(r->a)->Reader r a
asks f = Reader f


instance Applicative (Reader r) where
  pure :: a -> Reader r a
  pure a = Reader $ const a

  (<*>) :: Reader r (a -> b) -> Reader r a -> Reader r b
  (Reader rab) <*> (Reader ra) = Reader $ \r -> (rab r) (ra r)

newtype HumanName = HumanName String deriving (Eq, Show)
newtype DogName = DogName String deriving (Eq, Show)
newtype Address = Address String deriving (Eq, Show)

data Person = Person {humanName :: HumanName , dogName :: DogName , address :: Address} deriving (Eq, Show)
data Dog = Dog {dogsName :: DogName , dogsAddress :: Address } deriving (Eq, Show)

chris :: Person
chris = Person (HumanName "Chris Allen") (DogName "Papu") (Address "Austin")

-- getDogR :: Person -> Dog
-- getDogR = Dog <$> dogName <*> address

getDogR :: Reader Person Dog
getDogR = (Reader (Dog . dogName)) <*> (Reader address)

instance Monad (Reader r) where
  return = pure
  (>>=) :: Reader r a ->(a->Reader r b) -> Reader r b
  (Reader ra) >>= aRb = Reader $ (\r -> let (Reader rb) = aRb (ra r) in rb r)

getDogRM :: Reader Person Dog
getDogRM = do
    name <- Reader dogName
    addy <- Reader address
    return $ Dog name addy
