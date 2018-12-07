module StringProcess where

    import Data.List

    -- 1

    -- Function to check if the word is "the" or not
    notThe :: String -> String
    notThe string
        | string == "the" = "a"
        | otherwise = string

    -- Function to replace "the"s with "a"s
    replaceThe :: String -> String
    replaceThe "" = ""
    replaceThe sentence = concat $ intersperse " " $ map notThe $ words sentence

    -- 2 & 3

    -- Function to see if word starts with vowel
    startsWithVowel :: String -> Bool
    startsWithVowel [] = False
    startsWithVowel (x:xs) = elem x "aeiou"

    -- Function to count "the"s followed by a vowel
    countTheBeforeVowel :: String -> Integer
    countTheBeforeVowel string = go (words string) 0 where
        go [] count = count
        go (x:xs) count
            | x == "the" && (startsWithVowel (head xs)) = go (tail xs) (count + 1)
            | otherwise = go xs count

    -- Function to count vowel in string
    countVowel :: String -> Integer
    countVowel (x:xs) = go (x:xs) 0 where
        go [] count = count
        go (x:xs) count
            | elem x "aeiouAEIOU" = go xs (count + 1)
            | otherwise = go xs count