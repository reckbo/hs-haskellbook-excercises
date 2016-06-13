module Cipher where

import           Data.Char
import           Data.List       (intersperse)
import           Test.QuickCheck

charIndex :: Char -> Int
charIndex = subtract 97 . ord

caesarChar :: Int -> Char -> Char
caesarChar n = chr . (`mod` 256) . (n+) . ord

unCaesarChar :: Int -> Char -> Char
unCaesarChar n = chr . (`mod` 256) . subtract n . ord

caesar :: Int -> String -> String
caesar n = map (caesarChar n)

unCaesar :: Int -> String -> String
unCaesar n = map (unCaesarChar n)

--Vigenère cipher
type CipherKey = String
cipher :: CipherKey -> String -> String
cipher cipher msg = case cipher of
  "" -> msg
  _ -> f cipher msg
        where f = (map (uncurry caesarChar) .) . zip  . cycle . map charIndex

unCipher :: CipherKey -> String -> String
unCipher cipher msg = case cipher of
  "" -> msg
  _ -> f cipher msg
       where f = (map (uncurry unCaesarChar) .) . zip  . cycle . map charIndex

prop_identity :: Int -> String -> Bool
prop_identity n s = s == (finv . f) s where
                f = caesar n
                finv = unCaesar n

prop_identityCipher :: String -> String -> Bool
prop_identityCipher c s = s == (finv . f) s where
                      f = cipher c
                      finv = unCipher c

test :: IO ()
test = do
  -- verboseCheck prop_identity
    verboseCheck prop_identityCipher

main :: IO ()
main = do
    putStr "Enter string to be encrypted: "
    s <- getLine
    putStr "Enter number: "
    s' <- getLine
    let d = read s'
    putStrLn ""
    putStrLn $ caesar d s
