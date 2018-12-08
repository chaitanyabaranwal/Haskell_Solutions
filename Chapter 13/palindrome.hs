import Control.Monad
import System.Exit (exitSuccess)
import Data.Char

cleanString :: String -> String
cleanString string = filter (\x -> ord x > 96 && ord x < 123) (map toLower string)

palindrome :: IO()
palindrome = forever $ do
    line1 <- getLine
    case (cleanString line1 == reverse (cleanString line1)) of
        True -> putStrLn "It's a palindrome!"
        False -> do 
            putStrLn "Nope!"
            exitSuccess