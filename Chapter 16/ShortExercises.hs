module ShortExercises where

    -- Equivalent of Maybe
    data Possibly a = LolNope | Yeppers a deriving (Eq, Show)

    instance Functor Possibly where
        fmap f LolNope = LolNope
        fmap f (Yeppers a) = Yeppers (f a)

    -- Equivalent of Either
    data Sum a b = First a | Second b deriving (Eq, Show)
    
    instance Functor (Sum a) where
        fmap f (First a) = First a
        fmap f (Second b) = Second (f b)