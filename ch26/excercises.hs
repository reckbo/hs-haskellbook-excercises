newtype EitherT e m a =
  EitherT { runEitherT :: m (Either e a) }

instance Functor m => Functor (EitherT e m) where
  fmap f (EitherT me)= EitherT $ (fmap . fmap) f me

instance Applicative m => Applicative (EitherT e m) where
  pure = EitherT . pure . pure
  (EitherT mef) <*> (EitherT mea) = EitherT $ (<*>) <$> mef <*> mea

instance Monad m => Monad (EitherT e m) where
  return = pure
  (EitherT ma) >>= f = EitherT $ do
    ea <- ma
    case ea of
      (Left x) -> return $ Left x
      (Right x) -> runEitherT $ f x

swapEither :: Either e a -> Either a e
swapEither (Left x) = Right x
swapEither (Right x) = Left x

swapEitherT :: (Functor m) => EitherT e m a -> EitherT a m e
swapEitherT (EitherT mea) = EitherT $ fmap swapEither mea

eitherT :: Monad m => (a->m c) ->(b->m c) ->EitherT a m b -> m c
eitherT f g (EitherT mab) = do
  eab <- mab
  case eab of
    (Left x) -> f x
    (Right x) -> g x

newtype StateT s m a = StateT { runStateT :: s -> m (a,s) }

instance (Functor m) => Functor (StateT s m) where
  fmap f m = StateT $ \s -> fmap f' $ (runStateT m) s
                            where f' (a,s) = (f a, s)

instance (Monad m) => Applicative (StateT s m) where
  pure a = StateT $ \s -> return (a,s)
  xf <*> y = StateT $ \s -> let mf = runStateT xf $ s
                                ma = runStateT y $ s
                            in mf >>= \(f,_) ->
                                        ma >>= \(a,s') ->
                                                 return (f a, s')

instance (Monad m) => Monad (StateT s m) where
  return = pure
  sma >>= f = StateT $ \s -> do
    (a, _) <- runStateT sma s
    runStateT (f a) $ s

