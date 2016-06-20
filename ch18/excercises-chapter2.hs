import           Control.Monad

j :: Monad m => m (m a)-> m a
j x = x >>= id

l1 ::  (Functor m, Monad m) => (a->b)->m a->m b
l1 = fmap
-- l1 f m = m >>= (return . f)

l2 :: Monad m => (a->b->c)->m a->m b->m c
l2 f ma mb = ma >>= (\a ->
             mb >>= (\b -> return (f a b)))
  -- do a <- ma
  --               b <- mb
  --               return $ f a b

a :: Monad m => m a-> m (a->b)->m b
a ma mf = do a <- ma
             f <- mf
             return $ f a

meh :: Monad m => [a]->(a->m b)-> m [b]
meh xs f = foldr g (return []) (fmap f xs)
  where g mb mxs = do b <- mb
                      xs <- mxs
                      return $ b:xs

flipType :: (Monad m) => [m a] -> m [a]
flipType xs = meh xs id
