import StringProcess

newtype Word' = Word' String deriving (Eq, Show)
vowels = "aeiou"

mkWord :: String -> Maybe Word'
mkWord word
    | vowels > consonants = Nothing
    | otherwise = Just (Word' word)
    where
        vowels = countVowel word
        consonants = fromIntegral (length word) - vowels