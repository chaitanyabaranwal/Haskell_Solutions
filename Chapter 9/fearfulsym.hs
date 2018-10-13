myWords :: String -> Char -> [String]
myWords string char
    | string == "" = []
    | otherwise = [takeWhile (/= char) string] ++ (myWords (drop 1 (dropWhile (/= char) string)) char)