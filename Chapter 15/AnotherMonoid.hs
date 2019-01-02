module AnotherMonoid where

    import Data.Monoid
    import Test.QuickCheck

    -- Moniod laws
    monoidAssoc :: (Eq a, Monoid a) => a -> a -> a -> Bool
    monoidAssoc a b c = a <> (b <> c) == (a <> b) <> c

    monoidLeftIdentity :: (Eq a, Monoid a) => a -> Bool
    monoidLeftIdentity a = (mempty <> a) == a
    
    monoidRightIdentity :: (Eq a, Monoid a) => a -> Bool
    monoidRightIdentity a = (a <> mempty) == a

    -- Data constructors
    data Optional a = Nada | Only a deriving (Eq, Show)
    newtype First' a = First' { getFirst' :: Optional a } deriving (Eq, Show)

    instance Arbitrary a => Arbitrary (First' a) where
        arbitrary = do
            x <- arbitrary
            frequency [(1, return $ First' Nada), (1, return $ (First' (Only x)))]

    instance Monoid (First' a) where
        mempty = First' Nada
    instance Semigroup (First' a) where
        (First' Nada) <> (First' Nada) = First' Nada
        (First' Nada) <> (First' a) = First' a
        (First' a) <> _ = First' a

    type FirstMappend = First' String -> First' String -> First' String -> Bool
    type FstId = First' String -> Bool

    main :: IO()
    main = do
        quickCheck (monoidAssoc :: FirstMappend)
        quickCheck (monoidLeftIdentity :: FstId)
        quickCheck (monoidRightIdentity :: FstId)
        