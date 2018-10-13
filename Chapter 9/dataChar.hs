import Data.Char

takeUpper :: String -> String
takeUpper string = filter (\x -> isUpper x) string

capitalize :: String -> String
capitalize string = [toUpper $ head string] ++ (tail string)

toUpperString :: String -> String
toUpperString string
    | string == "" = ""
    | otherwise = [toUpper $ head string] ++ toUpperString (tail string)

firstLetter :: String -> Char
firstLetter = toUpper . head