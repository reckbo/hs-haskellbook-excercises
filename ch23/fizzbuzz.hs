import           Control.Monad
import           Control.Monad.Trans.State
import           Data.Foldable

fizzBuzz :: Integer -> String
fizzBuzz n | n`mod`15 == 0 = "FizzBuzz"
           | n`mod`5 ==0 = "Fizz"
           | n`mod`3 ==0 = "Buzz"
           | otherwise = show n

addResult :: Integer -> State [String] ()
addResult n = do
  xs <- get
  let result = fizzBuzz n
  put (result : xs)

fizzbuzzList :: [Integer] -> [String]
fizzbuzzList list = execState (mapM_ addResult list) []

fizzbuzzFromTo :: Integer -> Integer -> [String]
fizzbuzzFromTo from to = execState (sequenceA_ (go from to [])) []
  where
  go :: Integer -> Integer -> [State [String] ()] -> [State [String] ()]
  go l u xs =
    if l==u
    then (addResult u):xs
    else go (l+1) u (addResult l:xs)

main :: IO ()
main = traverse_ putStrLn $ fizzbuzzFromTo 1 100
