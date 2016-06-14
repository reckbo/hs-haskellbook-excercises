{-# LANGUAGE FlexibleInstances #-}
-- 1.

data Sum b a = First a | Second b
  deriving (Show)

instance Functor (Sum e) where
  fmap f (First a) = First (f a)
  fmap f (Second b) = Second b

-- 2.

data Company a c b = DeepBlue a c | Something b
  deriving Show

instance Functor (Company e e') where
  fmap f (Something b) = Something (f b)
  fmap _ (DeepBlue a c) = DeepBlue a c

-- 3.

data More b a = L a b a | R b a b
  deriving (Eq, Show)

instance Functor (More x) where
  fmap f (L a b a') = L (f a) b (f a')
  fmap f (R b a b') = R b (f a) b'

data Quant a b = Finance | Desk a | Bloor b
  deriving Show

instance Functor (Quant a) where
  fmap f (Bloor x) = Bloor $ f x
  fmap f Finance = Finance
  fmap f (Desk y) = Desk y

  -- 2.

data K a b = K a  deriving Show

instance Functor (K a) where
  fmap f (K x) = K x

  -- 3.

newtype Flip f a b = Flip (f b a)
  deriving (Eq, Show)

instance Functor (Flip K a) where
  fmap f (Flip (K x)) = Flip $ K $ f x

  -- 4.

data EvilGoateeConst a b = GoatyConst b
  deriving Show

instance Functor (EvilGoateeConst a) where
  fmap f (GoatyConst x) = GoatyConst $ f x

  -- 5.

data LiftItOut f a = LiftItOut (f a)
  deriving Show

instance Functor f => Functor (LiftItOut f) where
  fmap f (LiftItOut x) = LiftItOut $ fmap f x

  -- 6.

data Parappa f g a = DaWrappa (f a) (g a)
  deriving Show

instance (Functor g, Functor f) => Functor (Parappa f g) where
  fmap f (DaWrappa x x') = DaWrappa (fmap f x) (fmap f x')

  -- 7.

data IgnoreOne f g a b = IgnoringSomething (f a) (g b)
  deriving Show

instance Functor g => Functor (IgnoreOne f g a) where
  fmap f (IgnoringSomething x y) = IgnoringSomething x (fmap f y)

  -- 8.

data Notorious g o a t = Notorious (g o) (g a) (g t)
  deriving Show

instance Functor g => Functor (Notorious g o a) where
  fmap f (Notorious y y' x) = Notorious y y' (fmap f x)

  -- 9.

data List a = Nil | Cons a (List a)
  deriving Show

instance Functor List where
  fmap f (Cons x xs) = case xs of
    Nil -> Cons (f x) Nil
    otherwise -> Cons (f x) (fmap f xs)

  -- 10.

data GoatLord a =
  NoGoat |
  OneGoat a |
  MoreGoats (GoatLord a) (GoatLord a) (GoatLord a)
  deriving Show

instance Functor GoatLord where
  fmap f (OneGoat x) = OneGoat $ f x
  fmap f (MoreGoats x y z) = MoreGoats (fmap f x) (fmap f y) (fmap f z)
  fmap f NoGoat = NoGoat

  -- 11.

data TalkToMe a = Halt | Print String a | Read (String -> a)

instance Functor TalkToMe where
  fmap f (Print s x) = Print s (f x)
  fmap f Halt = Halt
  fmap f (Read g) = Read (f. g)
