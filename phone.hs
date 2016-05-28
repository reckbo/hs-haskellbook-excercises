import Data.Char (isUpper, toLower)

type Letters = String
data Button = Button Digit Letters deriving (Show)
type DaPhone = [Button]

mkButton :: Digit -> Letters -> Button
mkButton digit s = Button digit (s++[digit])

letterStrings :: [String]
letterStrings = [" ", "", "abc","def","ghi","jkl","mno","pqrs","tuv","wxyz", "^",".,"]

digits = "0123456789*#"

phone :: DaPhone
phone = map (uncurry mkButton) $ zip digits letterStrings

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
