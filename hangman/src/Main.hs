{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where
import           Control.Monad (forever)
import           Data.Char     (toLower)
import           Data.List     (intersperse)
import           Data.Maybe    (isJust)
import           System.Exit   (exitSuccess)
import           System.Random (randomRIO)
import           Test.Hspec

type WordList = [String]

allWords :: IO WordList
allWords = do
    dict <- readFile "data/dict.txt"
    return (lines dict)

minWordLength :: Int
minWordLength = 5
maxWordLength :: Int
maxWordLength = 9

gameWords :: IO WordList
gameWords = do
    aw <- allWords
    return (filter f aw)
    where f = inRange (minWordLength,maxWordLength) . length
                where inRange (m,n) i  =  m <= i && i <= n

randomWord :: WordList -> IO String
randomWord wl = do
    randomIndex <- randomRIO (0 , length wl)
    return $ wl !! randomIndex

randomWord' :: IO String
randomWord' = gameWords >>= randomWord

data Puzzle = Puzzle String [Maybe Char] [Char] deriving Eq

instance Show Puzzle where
    show (Puzzle _ discovered guessed) =
        intersperse ' ' $ fmap renderPuzzleChar discovered
        ++ " Guessed so far: " ++ guessed

renderPuzzleChar :: Maybe Char -> Char
renderPuzzleChar Nothing =  '_'
renderPuzzleChar (Just x) =  x

freshPuzzle :: String -> Puzzle
freshPuzzle s = Puzzle s guessedRight []
                    where guessedRight = replicate (length s) Nothing

charInWord :: Puzzle -> Char -> Bool
charInWord (Puzzle word _ _) ch = ch `elem` word

alreadyGuessed :: Puzzle -> Char -> Bool
alreadyGuessed (Puzzle _ _ guessed) ch = ch `elem` guessed

fillInCharacter :: Puzzle -> Char -> Puzzle
fillInCharacter (Puzzle word filledInSoFar s) c =
    Puzzle word newFilledInSoFar (c : s)
        where zipper guessed wordChar guessChar =
                if wordChar == guessed
                then Just wordChar
                else guessChar
              newFilledInSoFar = zipWith (zipper c) word filledInSoFar

handleGuess :: Puzzle -> Char -> IO Puzzle
handleGuess puzzle guess = do
    putStrLn $ "Your guess was: " ++ [guess]
    case (charInWord puzzle guess
            , alreadyGuessed puzzle guess) of
            (_, True) -> do
                putStrLn "You already guessed that\
                \ character, pick something else!"
                return puzzle
            (True, _) -> do
                putStrLn "This character was in the word,\
                \ filling in the word accordingly"
                return (fillInCharacter puzzle guess)
            (False, _) -> do
                putStrLn "This character wasn't in\
                \ the word, try again."
                return (fillInCharacter puzzle guess)

gameOver :: Puzzle -> IO ()
gameOver (Puzzle wordToGuess _ guessed) =
    if length guessed > 7
    then
        do putStrLn "You lose!"
           putStrLn $ "The word was: " ++ wordToGuess
           exitSuccess
    else return ()

gameWin :: Puzzle -> IO ()
gameWin (Puzzle _ filledInSoFar _) =
    if all isJust filledInSoFar
    then
        do putStrLn "You win!"
           exitSuccess
    else return ()

runGame :: Puzzle -> IO ()
runGame puzzle = forever $ do
    gameOver puzzle
    gameWin puzzle
    putStrLn $ "Current puzzle is: " ++ show puzzle
    putStr "Guess a letter: "
    guess <- getLine
    case guess of
        [c] -> handleGuess puzzle c >>= runGame
        _ ->putStrLn"Your guess must\
                    \ be a single character"

unitTests :: IO ()
unitTests = hspec $ do
  describe "fillInCharacter" $ do
    it "puppy with p" $ do
      (fillInCharacter (freshPuzzle "puppy") 'p') `shouldBe`
        Puzzle "puppy" [Just 'p', Nothing, Just 'p', Just 'p', Nothing] "p"
    it "kitty with k" $ do
      (fillInCharacter (freshPuzzle "kitty") 'k') `shouldBe`
        Puzzle "kitty" [Just 'k', Nothing, Nothing, Nothing, Nothing] "k"
    it "zebra with x" $ do
      (fillInCharacter (freshPuzzle "zebra") 'x') `shouldBe`
        Puzzle "zebra" [Nothing, Nothing, Nothing, Nothing, Nothing] "x"
  -- describe "handleGuess" $ do
  --   let p = Puzzle "bin" [Nothing,Nothing,Nothing] "z"
  --   p' <- handleGuess p 'z'
  --   p `shouldBe` p'

main :: IO ()
main = do
    word <- randomWord'
    let puzzle = freshPuzzle (fmap toLower word)
    runGame puzzle

