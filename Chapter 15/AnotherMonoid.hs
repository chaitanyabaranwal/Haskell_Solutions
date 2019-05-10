module AnotherMonoid where

    import Data.Monoid
    import Test.QuickCheck

    -- Declare newtype

    data Optional a = Nada | Only a deriving (Eq, Show)

    instance Semigroup a => Monoid (Optional a) where
        mempty = Nada
    instance Semigroup a => Semigroup (Optional a) where
        Nada <> (Only a) = Only a
        (Only a) <> Nada = Only a
        Nada <> Nada = Nada
        (Only a) <> (Only b) = Only (a <> b)
    instance Arbitrary a => Arbitrary (Optional a) where
        arbitrary = do
            a <- arbitrary
            elements [Only a, Nada]

    newtype First' a = First' { getFirst' :: Optional a } deriving (Eq, Show)
    
    instance Semigroup a => Monoid (First' a) where
        mempty = First' Nada
    instance Semigroup a => Semigroup (First' a) where
        (First' Nada) <> (First' Nada) = First' Nada
        First' (Only a) <> First' Nada = First' (Only a)
        First' Nada <> First' (Only a) = First' (Only a)
        First' (Only a) <> First' (Only b) = First' ((Only a) <> (Only b))
    instance Arbitrary a => Arbitrary (First' a) where
        arbitrary = do
            a <- arbitrary
            elements [First' (Only a), First' Nada]

    -- First' instance for concrete type

    firstMappend :: Monoid a => First' a -> First' a -> First' a
    firstMappend = (<>)
    
    type FirstMappend = First' String -> First' String -> First' String -> Bool
    type FirstId = First' String -> Bool

    -- Monoidal functions

    monoidAssoc :: (Eq a, Monoid a) => a -> a -> a -> Bool
    monoidAssoc a b c = (a <> b) <> c == a <> (b <> c)

    monoidLeftIdentity :: (Eq a, Monoid a) => a -> Bool
    monoidLeftIdentity a = (mempty <> a) == a

    monoidRightIdentity :: (Eq a, Monoid a) => a -> Bool
    monoidRightIdentity a = (a <> mempty) == a

    -- Main function

    main :: IO()
    main = do
        quickCheck (monoidAssoc :: FirstMappend)
        quickCheck (monoidLeftIdentity :: FirstId)
        quickCheck (monoidRightIdentity :: FirstId)
