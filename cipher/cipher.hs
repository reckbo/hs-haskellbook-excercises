module Cipher where

import Data.List (intersperse)
import Data.Char

charIndex :: Char -> Int
charIndex = subtract 97 . ord

caesarChar :: Int -> Char -> Char
caesarChar n = chr . (97+) . (`mod` 26) . (n+) . subtract 97 . ord

unCaesarChar :: Int -> Char -> Char
unCaesarChar n = chr . (97+) . (`mod` 26) . subtract n . subtract 97 . ord

caesar :: Int -> String -> String
caesar n = map (caesarChar n)

unCaesar :: Int -> String -> String
unCaesar n = map (unCaesarChar n)

--Vigenère cipher-}
type CipherKey = String
cipher :: CipherKey -> String -> String
cipher = (map (uncurry caesarChar) .) . zip  . cycle . map charIndex

main :: IO ()
main = do
    putStr "Enter string to be encrypted: "
    s <- getLine
    putStr "Enter number: "
    d <- read
    putStrLn ""
    putStrLn $ caesar d s
