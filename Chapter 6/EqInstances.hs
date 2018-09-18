-- 1

data TisAnInteger = TisAn Integer
instance Eq TisAnInteger where
    (==) (TisAn a) (TisAn b) = a == b

-- 2

data TwoIntegers = Two Integer Integer
instance Eq TwoIntegers where
    (==) (Two a b) (Two c d) = (a == c) && (b == d)

-- 3

data StringOrInt = TisAnInt Int | TisAString String
instance Eq StringOrInt where
    (==) (TisAnInt a) (TisAnInt b) = a == b
    (==) (TisAString c) (TisAString d) = c == d
    (==) _ _ = False

-- 4

data Pair a = Pair a a
instance Eq a => Eq (Pair a) where
    (==) (Pair c d) (Pair c' d') = (c == c') && (d == d')

-- 5

data Tuple a b = Tuple a b
instance (Eq a, Eq b) => Eq (Tuple a b) where
    (==) (Tuple c d) (Tuple c' d') = (c == c') && (d == d')

-- 6

data Which a = ThisOne a | ThatOne a
instance Eq a => Eq (Which a) where
    (==) (ThisOne c) (ThisOne d) = c == d
    (==) (ThatOne c') (ThatOne d') = c' == d'
    (==) _ _ = False

-- 7

data EitherOr a b = Hello a | Goodbye b
instance (Eq a, Eq b) => Eq (EitherOr a b) where
    (==) (Hello c) (Hello d) = c == d
    (==) (Goodbye c') (Goodbye d') = c' == d'
    (==) _ _ = False
