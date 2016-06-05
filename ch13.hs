import Control.Monad
import System.Exit (exitSuccess)
import Data.Char
import Data.Function (on)

clean :: String -> String
clean = map toLower . filter (/=' ') . filter (/='\'')  

palindrome :: IO () 
palindrome = forever $ do
    line1 <- getLine
    if ((==) `on` clean) line1 (reverse line1) 
    then putStrLn "It's a palindrome!" 
    else do putStrLn "Nope!" 
            exitSuccess

type Name          = String
type Age           = Integer
data Person        = Person Name Age deriving Show
data PersonInvalid = NameEmpty | AgeTooLow | PersonInvalidUnknown String deriving (Eq, Show)

mkPerson :: Name -> Age -> Either PersonInvalid Person 
mkPerson name age
    | name /= "" && age>0 = Right $ Person name age 
    | name == "" = Left NameEmpty
    | age <= 0 = Left AgeTooLow
    | otherwise = Left $ PersonInvalidUnknown $
                    "Name was: " ++ show name ++ 
                    "Agewas:"++show age

gimmePerson :: IO () 
gimmePerson = do 
    putStr "your name: "
    name <- getLine
    putStr "your age: "
    ageS <- getLine
    let age = read ageS :: Integer
    let person = mkPerson name age
    case person of
            (Right p ) -> print p
            (Left p) -> putStrLn $ "error: " ++ show p
