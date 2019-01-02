module SemiGroup where

    import Data.Semigroup
    import Test.QuickCheck

    -- Associativity for semigroup
    semigroupAssoc :: (Eq a, Semigroup a) => a -> a -> a -> Bool
    semigroupAssoc a b c = a <> (b <> c) == (a <> b) <> c

    -- 1
    data Trivial = Trivial deriving (Eq, Show)
    
    instance Semigroup Trivial where
        Trivial <> Trivial = Trivial
    instance Arbitrary Trivial where
        arbitrary = return Trivial

    type TrivialAssoc = Trivial -> Trivial -> Trivial -> Bool

    -- 2
    newtype Identity a = Identity a deriving (Eq, Show)

    instance Semigroup a => Semigroup (Identity a) where
        (Identity a) <> (Identity b) = Identity (a <> b)
    instance Arbitrary a => Arbitrary (Identity a) where
        arbitrary = do
            a <- arbitrary
            return (Identity a)
    
    type IdentityAssoc = Identity String -> Identity String -> Identity String -> Bool

    -- 3
    data Two a b = Two a b deriving (Eq, Show)

    instance (Semigroup a, Semigroup b) => Semigroup (Two a b) where
        (Two a b) <> (Two c d) = Two (a <> c) (b <> d)
    instance (Arbitrary a, Arbitrary b) => Arbitrary (Two a b) where
        arbitrary = do
            a <- arbitrary
            b <- arbitrary
            return (Two a b)

    type TwoAssoc = Two String String -> Two String String -> Two String String -> Bool

    -- 4
    data Three a b c = Three a b c deriving (Eq, Show)

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
    data Four a b c d = Four a b c d deriving (Eq, Show)

    instance (Semigroup a, Semigroup b, Semigroup c, Semigroup d) => Semigroup (Four a b c d) where
        (Four a b c d) <> (Four e f g h) = Four (a <> e) (b <> f) (c <> g) (d <> h)
    instance (Arbitrary a, Arbitrary b, Arbitrary c, Arbitrary d) => Arbitrary (Four a b c d) where
        arbitrary = do
            a <- arbitrary
            b <- arbitrary
            c <- arbitrary
            d <- arbitrary
            return (Four a b c d)

    type FourAssoc = Four String String String String -> Four String String String String -> Four String String String String -> Bool

    -- 6
    newtype BoolConj = BoolConj Bool deriving (Eq, Show)

    instance Semigroup BoolConj where
        (BoolConj a) <> (BoolConj b) = BoolConj (a && b)
    instance Arbitrary BoolConj where
        arbitrary = frequency [(1, return $ BoolConj True), (1, return $ BoolConj False)]

    type BoolConjAssoc = BoolConj -> BoolConj -> BoolConj -> Bool

    -- 7
    newtype BoolDisj = BoolDisj Bool deriving (Eq, Show)

    instance Semigroup BoolDisj where
        (BoolDisj a) <> (BoolDisj b) = BoolDisj (a || b)
    instance Arbitrary BoolDisj where
        arbitrary = frequency [(1, return $ BoolDisj True), (1, return $ BoolDisj False)]

    type BoolDisjAssoc = BoolDisj -> BoolDisj -> BoolDisj -> Bool

    -- 8
    data Or a b = Fst a | Snd b deriving (Eq, Show)

    instance (Arbitrary a, Arbitrary b) => Arbitrary (Or a b) where
        arbitrary = do
            a <- arbitrary
            b <- arbitrary
            frequency [(1, return $ Fst a), (1, return $ Snd b)]
    instance (Semigroup a, Semigroup b) => Semigroup (Or a b) where
        Fst a <> Fst b = Fst b
        Fst a <> Snd b = Snd b
        Snd a <> _ = Snd a

    type OrAssoc = Or String String -> Or String String -> Or String String -> Bool

    -- 9
    newtype Combine a b = Combine { unCombine :: a -> b }
    
    -- have to figure out Eq and Show instances for associativity to work out

    instance Semigroup b => Semigroup (Combine a b) where
        (Combine c) <> (Combine c') = Combine (c <> c')
    instance (CoArbitrary a, Arbitrary b) => Arbitrary (Combine a b) where
        arbitrary = do 
            f <- arbitrary
            return (Combine f)

    type CombineAssoc = Combine String String -> Combine String String -> Combine String String -> Bool

    -- 10
    newtype Comp a = Comp { unComp :: a -> a }

    -- have to figure out Eq and Show instances for associativity to work out

    instance Semigroup a => Semigroup (Comp a) where
        Comp b <> Comp b' = Comp (b <> b')
    instance (CoArbitrary a, Arbitrary a) => Arbitrary (Comp a) where
        arbitrary = do
            f <- arbitrary
            return (Comp f)

    -- 11
    data Validation a b = Failure' a | Success' b deriving (Eq, Show)

    instance (Arbitrary a, Arbitrary b) => Arbitrary (Validation a b) where
        arbitrary = do
            x <- arbitrary
            y <- arbitrary
            frequency [(1, return $ Failure' x), (1, return $ Success' y)]
    instance Semigroup a => Semigroup (Validation a b) where
        Failure' x <> Failure' y = Failure' (x <> y)
        Success' x <> _ = Success' x
        _ <> Success' x = Success' x

    type ValidationAssoc = Validation String Int -> Validation String Int -> Validation String Int -> Bool

    -- 12
    newtype AccumulateRight a b = AccumulateRight (Validation a b) deriving (Eq, Show)

    instance (Arbitrary a, Arbitrary b) => Arbitrary (AccumulateRight a b) where
        arbitrary = do
            x <- arbitrary
            y <- arbitrary
            frequency [(1, return $ AccumulateRight (Failure' x)), (1, return $ AccumulateRight (Success' y))]
    instance Semigroup b => Semigroup (AccumulateRight a b) where
        AccumulateRight (Failure' x) <> _ = AccumulateRight (Failure' x)
        _ <> AccumulateRight (Failure' x) = AccumulateRight (Failure' x)
        AccumulateRight (Success' x) <> AccumulateRight (Success' y) = AccumulateRight $ Success' (x <> y)

    type AccumulateRightAssoc = AccumulateRight Int String -> AccumulateRight Int String -> AccumulateRight Int String -> Bool

    -- 13
    newtype AccumulateBoth a b = AccumulateBoth (Validation a b) deriving (Eq, Show)

    instance (Arbitrary a, Arbitrary b) => Arbitrary (AccumulateBoth a b) where
        arbitrary = do
            x <- arbitrary
            y <- arbitrary
            frequency [(1, return $ AccumulateBoth (Failure' x)), (1, return $ AccumulateBoth (Success' y))]
    instance (Semigroup a, Semigroup b) => Semigroup (AccumulateBoth a b) where
        AccumulateBoth (Failure' x) <> AccumulateBoth (Failure' y) = AccumulateBoth $ Failure' (x <> y)
        AccumulateBoth (Failure' x) <> _ = AccumulateBoth (Failure' x)
        _ <> AccumulateBoth (Failure' x) = AccumulateBoth (Failure' x)
        AccumulateBoth (Success' x) <> AccumulateBoth (Success' y) = AccumulateBoth $ Success' (x <> y)

    type AccumulateBothAssoc = AccumulateBoth [Int] String -> AccumulateBoth [Int] String -> AccumulateBoth [Int] String -> Bool

    -- Main function
    main :: IO()
    main = do
        quickCheck (semigroupAssoc :: TrivialAssoc)
        quickCheck (semigroupAssoc :: IdentityAssoc)
        quickCheck (semigroupAssoc :: TwoAssoc)
        quickCheck (semigroupAssoc :: ThreeAssoc)
        quickCheck (semigroupAssoc :: FourAssoc)
        quickCheck (semigroupAssoc :: BoolConjAssoc)
        quickCheck (semigroupAssoc :: BoolDisjAssoc)
        quickCheck (semigroupAssoc :: OrAssoc)
        -- quickCheck (semigroupAssoc :: CombineAssoc)
        quickCheck (semigroupAssoc :: ValidationAssoc)
        quickCheck (semigroupAssoc :: AccumulateRightAssoc)
        quickCheck (semigroupAssoc :: AccumulateBothAssoc)

