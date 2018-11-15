{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}

class TooMany a where
    tooMany :: a -> Bool
instance TooMany Int where
    tooMany n = n > 42

newtype Goats = Goats Int deriving (Eq, Show, TooMany)

-- 1
newtype Pair = Pair (Int, String) deriving Show
instance TooMany Pair where
    tooMany (Pair (x, _)) = tooMany x

-- 2
newtype Group = Group (Int, Int) deriving Show
instance TooMany Group where
    tooMany (Group (x, y)) = tooMany $ x + y

-- 3
instance (Num a, TooMany a) => TooMany (a, a) where
    tooMany (x, y) = tooMany $ x + y