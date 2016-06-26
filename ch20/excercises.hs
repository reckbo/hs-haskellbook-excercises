import           Data.Monoid

sum' :: (Foldable t, Num a) => t a -> a
sum' = getSum . foldMap Sum

product' :: (Foldable t, Num a) => t a -> a
product' = getProduct . foldMap Product

elem' :: (Foldable t, Eq a) => a -> t a -> Bool
elem' x = getAny . foldMap (Any .(==x))

minimum' :: (Foldable t, Ord a) => t a -> Maybe a
minimum' = foldr f' Nothing
            where f' a b = Just (case b of
                                   Nothing -> a
                                   Just x -> min a x)

maximum' :: (Foldable t, Ord a) => t a -> Maybe a
maximum' = foldr f' Nothing
            where f' a b = Just (case b of
                                   Nothing -> a
                                   Just x -> max a x)

null' :: (Foldable t) => t a -> Bool
null' t = case (foldr (\_ _ -> Just ()) Nothing t) of
              Nothing -> True
              _ -> False

length' :: (Foldable t) => t a -> Int
length' = foldr (\_ b -> 1+b) 0

toList' :: (Foldable t) => t a -> [a]
toList' = foldMap (:[]) 

fold' :: (Foldable t, Monoid m) => t m -> m
fold' = foldMap id

foldMap' :: (Foldable t, Monoid m) => (a -> m) -> t a -> m
foldMap' f = foldr (\a b -> f a <> b) mempty


