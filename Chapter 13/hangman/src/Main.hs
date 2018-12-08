module Main where

  import Control.Monad (forever)
  import Data.Char (toLower)
  import Data.Maybe (isJust)
  import Data.List (intersperse)
  import System.Exit (exitSuccess)
  import System.Random (randomRIO)

  newtype WordList = WordList [String] deriving (Eq, Show)
  
  allWords :: IO WordList
  allWords = do
    dict <- readFile "data/dict.txt"
    return $ WordList (lines dict)

  minWordLength :: Int
  minWordLength = 5
  maxWordLength :: Int
  maxWordLength = 10

  gameWords :: IO WordList
  gameWords = do
    (WordList aw) <- allWords
    return $ WordList (filter gameLength aw) 
      where gameLength w = let l = length (w :: String) in l >= minWordLength && l <= maxWordLength

  randomWord :: WordList -> IO String
  randomWord (WordList wl) = do
    randomIndex <- randomRIO(0, (length wl) - 1)
    return (wl !! randomIndex)

  randomWord' :: IO String
  randomWord' = gameWords >>= randomWord

  data Puzzle = Puzzle String [Maybe Char] [Char] Integer
  instance Show Puzzle where
    show (Puzzle _ discovered guessed wrongCount) =
      (intersperse ' ' $ fmap renderPuzzleChar discovered)
      ++ " Guessed letters: " ++ guessed

  freshPuzzle :: String -> Puzzle
  freshPuzzle word = Puzzle word (map (const Nothing) word) [] 0

  charInWord :: Puzzle -> Char -> Bool
  charInWord (Puzzle word _ _ _) char = elem char word

  alreadyGuessed :: Puzzle -> Char -> Bool
  alreadyGuessed (Puzzle _ _ guessed _) char = elem char guessed

  increaseWrongCount :: Puzzle -> Puzzle
  increaseWrongCount (Puzzle word filledInSoFar guessed wrongCount) =
    Puzzle word filledInSoFar guessed (wrongCount + 1)

  renderPuzzleChar :: Maybe Char -> Char
  renderPuzzleChar char = case char of
    Nothing -> '_'
    (Just char) -> char

  fillInChar :: Puzzle -> Char -> Puzzle
  fillInChar (Puzzle word filledInSoFar s wrongCount) c =
    (Puzzle word newFilledInSoFar (c:s) wrongCount) where
      zipper guessed wordChar guessChar =
        if guessed == wordChar then (Just wordChar) else guessChar
      newFilledInSoFar = zipWith (zipper c) word filledInSoFar

  handleGuess :: Puzzle -> Char -> IO Puzzle
  handleGuess puzzle guess = do
    putStrLn $ "Your guess was: " ++ [guess]
    case (charInWord puzzle guess, alreadyGuessed puzzle guess) of
      (_, True) -> do 
        putStrLn "You already guessed that\
        \ character, guess something else!"
        return puzzle
      (True, _) -> do
        putStrLn "You guessed a correct letter!"
        return (fillInChar puzzle guess)
      (False, _) -> do
        putStrLn "That character does not exist in the word :("
        return (fillInChar (increaseWrongCount puzzle) guess)

  gameOver :: Puzzle -> IO()
  gameOver (Puzzle word _ guessed wrongCount) =
    if (wrongCount > 7) then
      do 
        putStrLn "You lose!"
        putStrLn $ "The word is: " ++ word
        exitSuccess
    else return ()

  gameWin :: Puzzle -> IO()
  gameWin (Puzzle word filledInSoFar _ _) =
    if all isJust filledInSoFar then
      do 
        putStrLn "You win!"
        exitSuccess
    else return ()

  runGame :: Puzzle -> IO()
  runGame puzzle = forever $ do
    gameOver puzzle
    gameWin puzzle
    putStrLn $ "Current puzzle is: " ++ show puzzle
    putStrLn "Guess a letter: "
    guess <- getLine
    case guess of
      [c] -> handleGuess puzzle c >>= runGame
      _ -> putStrLn "Your guess must be a single character."

  main :: IO()
  main = do
    word <- randomWord'
    let puzzle = freshPuzzle (fmap toLower word)
    runGame puzzle
