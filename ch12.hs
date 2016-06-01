newtype Word' = Word' String deriving (Eq, Show)
vowels = "aeiou"


countLetterTypes :: String -> (Int,Int)
countLetterTypes s = count 0 0 s
    where 
        count nc nv [] = (0,0)
        count nc nv (x:[]) = if x `elem` vowels then (nc,nv+1) else (nc+1,nv)
        count nc nv (x:xs) 
                    | x `elem` vowels = count nc (nv+1) xs
                    | otherwise = count (nc+1) nv xs

mkWord :: String -> Maybe Word'
mkWord s
    | nv > nc = Nothing
    | otherwise = Just $ Word' s
    where (nc,nv) = countLetterTypes s

data Nat = Zero | Succ Nat 
    deriving (Eq, Show)

natToInteger :: Nat -> Integer 
natToInteger Zero = 0
natToInteger (Succ x) = 1 + natToInteger x

integerToNat :: Integer -> Maybe Nat 
integerToNat x 
        | x < 0 = Nothing
        | x == 0 = Just Zero
        | otherwise = Just $ unravel x 
            where 
            unravel 0 = Zero
            unravel x = Succ $ unravel $ x-1


isJust :: Maybe a -> Bool
isJust (Just _) = True
isJust Nothing = False

isNothing :: Maybe a -> Bool
isNothing = not . isJust

mayybee :: b->(a->b)->Maybe a->b
mayybee b f Nothing = b
mayybee b f (Just x) = f x

fromMaybe :: a -> Maybe a -> a
fromMaybe x Nothing = x
fromMaybe x (Just x') = x'

listToMaybe :: [a] -> Maybe a
listToMaybe [] = Nothing
listToMaybe x = Just . head $ x

maybeToList :: Maybe a -> [a]
maybeToList Nothing = []
maybeToList (Just x) = [x]

catMaybes :: [Maybe a] -> [a]
catMaybes = concat . map maybeToList

flipMaybe :: [Maybe a] -> Maybe [a]
flipMaybe xs = case (length xs) > (length as) of
                True -> Nothing
                False -> Just as
            where as = catMaybes xs


lefts' :: [Either a b] -> [a]
lefts' = foldr f [] 
        where f (Left y) = (y:)
              f (Right _) = id
              
rights' :: [Either a b] -> [b]
rights' = foldr f [] 
        where f (Left _) = id
              f (Right y) = (y:)

partitionEithers' :: [Either a b] -> ([a], [b])
partitionEithers' x = ((lefts' x), (rights' x))

eitherMaybe' :: (b -> c) -> Either a b -> Maybe c
eitherMaybe' _ (Left _) = Nothing
eitherMaybe' f (Right x) = Just $ f x

either' :: (a->c) -> (b->c) -> Either a b -> c
either' f _ (Left x) = f x
either' _ f (Right x) = f x

eitherMaybe'' :: (b -> c) -> Either a b -> Maybe c
eitherMaybe'' f = either' (const $ Nothing) (Just . f)

myIterate :: (a -> a) -> a -> [a]
myIterate f x = y : myIterate f y
                where y = f x

myUnfoldr :: (b -> Maybe (a, b)) -> b -> [a] 
myUnfoldr f x = case f x of
                Nothing -> []
                Just (a,b) -> a : myUnfoldr f b

betterIterate :: (a -> a) -> a -> [a] 
betterIterate f x = myUnfoldr (\x -> Just (x, f x)) x


data BinaryTree a = Leaf | 
                    Node (BinaryTree a) a (BinaryTree a) 
                    deriving (Eq, Ord, Show)

unfold :: (a -> Maybe (a,b,a)) -> a -> BinaryTree b
unfold f x = case f x of
                Nothing -> Leaf
                Just (x',y,x'') -> Node (unfold f x') y (unfold f x'')

treeBuild :: Integer -> BinaryTree Integer 
treeBuild n = unfold f 0 
            where f n' 
                    | n' == n = Nothing
                    | otherwise = Just (n'+1,n',n'+1)
                
