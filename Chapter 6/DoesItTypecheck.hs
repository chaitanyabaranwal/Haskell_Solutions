-- 1

data Person = Person Bool deriving Show

printPerson :: Person -> IO ()
printPerson person = putStrLn (show person)

-- 2

data Mood = Blah | Woot deriving (Show, Eq)
settleDown x = if x == Woot then Blah else x

-- 3

type Subject = String
type Verb = String
type Object = String
data Sentence = Sentence Subject Verb Object deriving (Eq, Show)
s1 = Sentence "dogs" "drool" -- won't be able to print and need another string
s2 = Sentence "Julie" "loves" "dogs"