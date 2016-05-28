import Data.Char (isUpper, toLower)
import Data.List

--------
-- Given
--------

-- validButtons = "1234567890*#"
type Digit = Char
-- Valid presses: 1 and up
type Presses = Int

convo :: [String]
convo =
       ["Wanna play 20 questions",
        "Ya",
        "U 1st haha",
        "Lol ok. Have u ever tasted alcohol lol",
        "Lol ya",
        "Wow ur cool haha. Ur turn",
        "Ok. Do u think I am pretty Lol",
        "Lol ya",
        "Haha thanks just making sure rofl ur turn"]


----------
-- Types
----------
type Msg = String
type Letters = String
data Button = Button Digit Letters deriving (Show)
type DaPhone = [Button]

--------
-- Data
--------
letterGroups = [" ", "", "abc","def","ghi","jkl","mno","pqrs","tuv","wxyz", "^",".,"]
digits = "0123456789*#"

mkButton :: Digit -> Letters -> Button
mkButton digit s = Button digit (s++[digit])

phone :: DaPhone
phone = map (uncurry mkButton) $ zip digits letterGroups

-----------
-- Answers
-----------

numPresses :: Char -> Button -> (Digit, Int)
numPresses ch (Button digit letters) = (digit, press 1 ch letters)
        where press n ch [] = 0
              press n ch (x:xs) = if ch==x then n else press (n+1) ch xs

reverseTaps :: DaPhone -> Char -> [(Digit, Presses)]
reverseTaps phone ch 
        | isUpper ch = cap:[btn]
        | otherwise = [btn]
        where 
            btn = head $ filter ((0<) . snd) $ map (numPresses (toLower ch)) phone
            cap = head $ filter ((0<) . snd) $ map (numPresses '^') phone


cellPhonesDead :: DaPhone -> String -> [(Digit, Presses)] 
cellPhonesDead phone = concat . map (reverseTaps phone)

fingerTaps :: [(Digit, Presses)] -> Presses 
fingerTaps = sum . (map snd) 

mostPopularLetter :: String -> Char
mostPopularLetter msg = head $ head $ sortOn (negate.length) $ groupBy (==) $ sort msg

numChars :: Char -> String -> Int
numChars ch = length . filter (==ch)

charCost :: DaPhone -> Char -> String -> Int
charCost phone ch msg = count * letterCost
        where count = numChars ch msg
              letterCost = fingerTaps $ reverseTaps phone ch 

coolestLtr :: [String] -> Char 
coolestLtr = mostPopularLetter . concat 


coolestWord :: [String] -> String
coolestWord convo = head $ head $ sortOn (negate.length) $ group $ sort $  map capString $ concat $ map words convo
                where capString = map toLower
                      
