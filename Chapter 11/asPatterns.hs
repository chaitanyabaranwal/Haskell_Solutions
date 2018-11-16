import Data.Char
import Data.List

-- 1

issubsequenceOf :: (Eq a) => [a] -> [a] -> Bool
issubsequenceOf [] _ = True
issubsequenceOf _ [] = False
issubsequenceOf xs@(x: _) ys@(y: _)
    | x == y = issubsequenceOf (tail xs) (tail ys)
    | otherwise = issubsequenceOf xs (tail ys)

-- 2

f a@(x:xs) = (a, (toUpper x):xs)

capitalizeWords :: String -> [(String, String)]
capitalizeWords = map f . words

-- 3

capitalizeWord :: String -> String
capitalizeWord (x:xs) = (toUpper x):xs

-- 4

getSentences :: String -> [String]
getSentences string
    | string == "" = []
    | otherwise = [(takeWhile (/= '.') string)] ++ (getSentences (drop 2 (dropWhile (/= '.') string)))

capitalizePara :: String -> String
capitalizePara string = (concat $ intersperse ". " (map capitalizeWord $ getSentences string)) ++ "."