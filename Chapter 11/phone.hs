import Data.Char
import Data.List

-- Function to get index of element in list
search :: Eq a => a -> [a] -> Int
search x y = case index of
    Just v -> v + 1
    Nothing -> 0
    where index = elemIndex x y

-- Data structure for phone representation
data DaPhone = DaPhone [(Char, String)] deriving (Eq, Show)
daPhone = DaPhone
    [('1', "1"),
    ('2', "abc2"),
    ('3', "def3"),
    ('4', "ghi4"),
    ('5', "jkl5"),
    ('6', "mno6"),
    ('7', "pqrs7"),
    ('8', "tuv8"),
    ('9', "wxyz9"),
    ('*', "^"),
    ('0', " +_"),
    ('#', ".,")]

-- List of strings
convo :: [String]
convo =
    ["Wanna play 20 questions",
    "Ya",
    "U 1st haha",
    "Lol ok. Have u ever tasted alcohol lol",
    "Lol ya",
    "Wow ur cool haha. Ur turn",
    "Ok. Do u think I am pretty Lol",
    "Lol ya",
    "Haha thanks just making sure rofl ur turn"]

type Digit = Char
type Presses = Int

-- Function to get a list of presses for each character
reverseTaps :: DaPhone -> Char -> [(Digit, Presses)]
reverseTaps daPhone@(DaPhone (x:xs)) charNeeded
    | isUpper charNeeded = [('*', 1)] ++ reverseTaps daPhone (toLower charNeeded)
    | daPhone == (DaPhone []) = undefined
    | elem charNeeded (snd x) = [(fst x, search charNeeded (snd x))]
    | not (elem charNeeded (snd x)) = reverseTaps (DaPhone xs) charNeeded

-- Function to get keypresses for a string
getTaps :: String -> [(Digit, Presses)]
getTaps "" = []
getTaps (x:xs) = reverseTaps daPhone x ++ getTaps xs

-- Function to get total presses
fingerTaps :: [(Digit, Presses)] -> Presses
fingerTaps [] = 0
fingerTaps (x:xs) = snd x + fingerTaps xs

-- Function to get most popular letter
getLetterCounts :: String -> [(Char, Int)]
getLetterCounts string = map (\x -> (head x, length x)) $ group $ sort string

mostPopularLetter :: String -> Char
mostPopularLetter string = go (x:xs) x where
    go [] max = fst max
    go (x:xs) max
        | snd x > snd max = go xs x
        | otherwise = go xs max
    (x:xs) = getLetterCounts string

-- Cost of most popular letter
mostPopularLetterCost :: String -> Presses
mostPopularLetterCost string = go (x:xs) y 0 where
    go [] _ count = count
    go (x:xs) y count
        | elem x y = go xs y (count + snd x)
        | otherwise = go xs y count
    (x:xs) = getTaps string
    y = reverseTaps daPhone (mostPopularLetter string)

-- Most popular letter overall
coolestLtr :: [String] -> Char
coolestLtr convo = mostPopularLetter $ filter (/= ' ') $ concat convo

-- Most popular word overall
lowerString = map toLower

coolestWord :: [String] -> String
coolestWord convo = go (x:xs) x where
    go [] max = fst max
    go (x:xs) max
        | snd x > snd max = go xs x
        | otherwise = go xs max
    (x:xs) = map (\x -> (head x, length x)) $ group $ words $ lowerString (concat (intersperse " " convo))