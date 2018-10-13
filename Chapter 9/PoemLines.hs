module PoemLines where

    myLines :: String -> Char -> [String]
    myLines string char
        | string == "" = []
        | otherwise = [takeWhile (/= char) string] ++ (myLines (drop 1 (dropWhile (/= char) string)) char)