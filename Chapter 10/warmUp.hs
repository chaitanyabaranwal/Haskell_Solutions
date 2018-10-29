-- 1
stops = ["Hello", "Hi"]
vowels = ["World", "You"]

---- (a)
wordCombos :: [Char] -> [Char] -> [(Char, Char, Char)]
wordCombos stops vowels = [(a, b, c) | a <- stops, b <- vowels, c <- stops]

---- (b)
wordCombos2 :: [Char] -> [Char] -> [(Char, Char, Char)]
wordCombos2 stops vowels = [(a, b, c) | a <- stops, b <- vowels, c <- stops, a == 'p']

---- (c)
wordCombos3 :: [String] -> [String] -> [(String, String, String)]
wordCombos3 nouns verbs = [(a, b, c) | a <- nouns, b <- verbs, c <- nouns]

-- 2
-- Function finds the average word length in a sentence
seekritFunc :: String -> Double
seekritFunc x = (/) (fromIntegral $ sum (map length (words x))) (fromIntegral $ length (words x))