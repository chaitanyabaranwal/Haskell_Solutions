module SemigroupEx where

    import Test.QuickCheck
    import Test.QuickCheck.Gen (oneof)
    import Data.Monoid

    -- Semigroup requirement
    semigroupAssoc :: (Eq a, Semigroup a) => a -> a -> a -> Bool
    semigroupAssoc a b c = a <> (b <> c) == (a <> b) <> c

    -- 1
    data Trivial = Trivial deriving (Eq, Show)
    instance Semigroup Trivial where
        _ <> _ = Trivial
    instance Arbitrary Trivial where
        arbitrary = return Trivial
    type TrivialAssoc = Trivial -> Trivial -> Trivial -> Bool

    -- 2
    newtype Identity a = Identity a deriving (Show)
    instance Eq a => Eq (Identity a) where
        (==) (Identity a) (Identity b) = a == b
    instance Semigroup a => Semigroup (Identity a) where
        (Identity a) <> (Identity b) = Identity (a <> b)
    instance Arbitrary a => Arbitrary (Identity a) where
        arbitrary = do
            a <- arbitrary
            return (Identity a)
    type IdentityAssoc = Identity String -> Identity String -> Identity String -> Bool

    -- 3
    data Two a b = Two a b deriving (Show)
    instance (Eq a, Eq b) => Eq (Two a b) where
        (==) (Two a b) (Two c d) = (a == c) && (b == d)
    instance (Semigroup a, Semigroup b) => Semigroup (Two a b) where
        (Two a b) <> (Two c d) = Two (a <> c) (b <> d)
    instance (Arbitrary a, Arbitrary b) => Arbitrary (Two a b) where
        arbitrary = do
            a <- arbitrary
            b <- arbitrary
            return (Two a b)
    type TwoAssoc = Two String String -> Two String String -> Two String String -> Bool

    -- 4
    data Three a b c = Three a b c deriving (Show)
    instance (Eq a, Eq b, Eq c) => Eq (Three a b c) where
        (==) (Three a b c) (Three d e f) = (a == d) && (b == e) && (c == f)
    instance (Semigroup a, Semigroup b, Semigroup c) => Semigroup (Three a b c) where
        (Three a b c) <> (Three d e f) = Three (a <> d) (b <> e) (c <> f)
    instance (Arbitrary a, Arbitrary b, Arbitrary c) => Arbitrary (Three a b c) where
        arbitrary = do
            a <- arbitrary
            b <- arbitrary
            c <- arbitrary
            return (Three a b c)
    type ThreeAssoc = Three String String String -> Three String String String -> Three String String String -> Bool

    -- 5
    -- Similar as the two above, too tedious to write

    -- 6
    newtype BoolConj = BoolConj Bool deriving (Eq, Show)
    instance Semigroup BoolConj where
        BoolConj False <> _ = BoolConj False
        _ <> BoolConj False = BoolConj False
        BoolConj True <> BoolConj True = BoolConj True
    instance Arbitrary BoolConj where
        arbitrary = elements [BoolConj True, BoolConj False]
    type BoolConjAssoc = BoolConj -> BoolConj -> BoolConj -> Bool


    -- 7
    newtype BoolDisj = BoolDisj Bool deriving (Eq, Show)
    instance Semigroup BoolDisj where
        BoolDisj True <> _ = BoolDisj True
        _ <> BoolDisj True = BoolDisj True
        BoolDisj False <> BoolDisj False = BoolDisj False
    instance Arbitrary BoolDisj where
        arbitrary = elements [BoolDisj True, BoolDisj False]
    type BoolDisjAssoc = BoolDisj -> BoolDisj -> BoolDisj -> Bool

    -- 8
    data Or a b = Fst a | Snd b deriving (Show)
    instance (Eq a, Eq b) => Eq (Or a b) where
        (==) (Fst a) (Fst b) = a == b
        (==) (Snd a) (Snd b) = a == b
        (==) _ _ = False
    instance (Semigroup a, Semigroup b) => Semigroup (Or a b) where
        (Snd a) <> _ = Snd a
        _ <> (Snd a) = Snd a
        (Fst a) <> (Fst b) = Fst b
    instance (Arbitrary a, Arbitrary b) => Arbitrary (Or a b) where
        arbitrary = do
            a <- arbitrary
            b <- arbitrary
            oneof [return $ Fst a, return $ Snd b]
    type OrAssoc = Or String String -> Or String String -> Or String String -> Bool

    -- 9
    -- Still need to figure out how to make the Show and Eq instances more generalised
    newtype Combine a b = Combine { unCombine :: (a -> b) }
    instance (Num a, Eq b) => Eq (Combine a b) where
        (==) (Combine c) (Combine c') = (c $ 10) == (c' $ 10)
    instance (Num a, Show b) => Show (Combine a b) where
        show (Combine c) = "Combine: " ++ show (c 10)
    instance Semigroup b => Semigroup (Combine a b) where
        Combine c <> Combine c' = Combine (c <> c')
    instance (CoArbitrary a, Arbitrary b) => Arbitrary (Combine a b) where
        arbitrary = do
            f <- arbitrary
            return (Combine f)
    type CombineAssoc = Combine Int String -> Combine Int String -> Combine Int String -> Bool

    -- 10
    -- Still need to figure out how to make the Show and Eq instances more generalised
    newtype Comp a = Comp { unComp :: (a -> a) }
    -- instance (Eq a) => Eq (Comp a) where
    --     (==) (Comp b) (Comp b') = (b "test") == (b' "test")
    -- instance (Show a) => Show (Comp a) where
    --     show (Comp b) = "Comp: " ++ show (b "test")
    instance Semigroup a => Semigroup (Comp a) where
        Comp b <> Comp b' = Comp (b <> b')
    instance (CoArbitrary a, Arbitrary a) => Arbitrary (Comp a) where
        arbitrary = do
            f <- arbitrary
            return (Comp f)
    type CompAssoc = Comp String -> Comp String -> Comp String -> Bool

    -- 11
    data Validation a b = Success' a | Failure' b deriving (Eq, Show)
    instance (Semigroup a, Semigroup b) => Semigroup (Validation a b) where
        Failure' b <> Success' a = Failure' b
        Success' a <> Failure' b = Failure' b
        Success' a <> Success' b = Success' (a <> b)
        Failure' a <> Failure' b = Failure' (a <> b)
    instance (Arbitrary a, Arbitrary b) => Arbitrary (Validation a b) where
        arbitrary = do
            a <- arbitrary
            b <- arbitrary
            oneof [return $ Success' a, return $ Failure' b]
    type ValidationAssoc = Validation String String -> Validation String String -> Validation String String -> Bool

    -- 12
    newtype AccumulateRight a b = AccumulateRight (Validation a b) deriving (Eq, Show)
    instance Semigroup b => Semigroup (AccumulateRight a b) where
        AccumulateRight (Failure' b) <> AccumulateRight (Success' a) = AccumulateRight (Failure' b)
        AccumulateRight (Success' a) <> AccumulateRight (Failure' b) = AccumulateRight (Failure' b)
        AccumulateRight (Success' a) <> AccumulateRight (Success' b) = AccumulateRight (Success' b)
        AccumulateRight (Failure' a) <> AccumulateRight (Failure' b) = AccumulateRight (Failure' b)
    instance (Arbitrary a, Arbitrary b) => Arbitrary (AccumulateRight a b) where
        arbitrary = do
            a <- arbitrary
            b <- arbitrary
            oneof [return $ AccumulateRight (Success' a), return $ AccumulateRight (Failure' b)]
    type AccumulateRightAssoc = AccumulateRight String String -> AccumulateRight String String -> AccumulateRight String String -> Bool

    -- 13
    newtype AccumulateBoth a b = AccumulateBoth (Validation a b) deriving (Eq, Show)
    instance (Semigroup b, Semigroup a) => Semigroup (AccumulateBoth a b) where
        AccumulateBoth (Failure' b) <> AccumulateBoth (Success' a) = AccumulateBoth (Failure' b)
        AccumulateBoth (Success' a) <> AccumulateBoth (Failure' b) = AccumulateBoth (Failure' b)
        AccumulateBoth (Success' a) <> AccumulateBoth (Success' b) = AccumulateBoth (Success' (a <> b))
        AccumulateBoth (Failure' a) <> AccumulateBoth (Failure' b) = AccumulateBoth (Failure' (a <> b))
    instance (Arbitrary a, Arbitrary b) => Arbitrary (AccumulateBoth a b) where
        arbitrary = do
            a <- arbitrary
            b <- arbitrary
            oneof [return $ AccumulateBoth (Success' a), return $ AccumulateBoth (Failure' b)]
    type AccumulateBothAssoc = AccumulateBoth String String -> AccumulateBoth String String -> AccumulateBoth String String -> Bool

    -- Main function
    main :: IO()
    main = do
        putStrLn "Testing Trivial..."
        quickCheck (semigroupAssoc :: TrivialAssoc)
        putStrLn "Testing Identity..."
        quickCheck (semigroupAssoc :: IdentityAssoc)
        putStrLn "Testing Two..."
        quickCheck (semigroupAssoc :: TwoAssoc)
        putStrLn "Testing Three..."
        quickCheck (semigroupAssoc :: ThreeAssoc)
        putStrLn "Testing BoolConj..."
        quickCheck (semigroupAssoc :: BoolConjAssoc)
        putStrLn "Testing BoolDisj..."
        quickCheck (semigroupAssoc :: BoolDisjAssoc)
        putStrLn "Testing Or..."
        quickCheck (semigroupAssoc :: OrAssoc)
        putStrLn "Testing Combine..."
        quickCheck (semigroupAssoc :: CombineAssoc)
        -- putStrLn "Testing Comp..."
        -- quickCheck (semigroupAssoc :: CompAssoc)
        putStrLn "Testing Validation..."
        quickCheck (semigroupAssoc :: ValidationAssoc)
        putStrLn "Testing AccumulateRight..."
        quickCheck (semigroupAssoc :: AccumulateRightAssoc)
        putStrLn "Testing AccumulateBoth..."
        quickCheck (semigroupAssoc :: AccumulateBothAssoc)

