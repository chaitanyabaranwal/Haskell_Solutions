module MonoidEx where

    import Data.Monoid
    import Test.QuickCheck

    -- Associativity for semigroup
    monoidAssoc :: (Eq a, Monoid a) => a -> a -> a -> Bool
    monoidAssoc x y z = x <> (y <> z) == (x <> y) <> z

    monoidLeftIdentity :: (Eq a, Monoid a) => a -> Bool
    monoidLeftIdentity x = (mempty <> x) == x

    monoidRightIdentity :: (Eq a, Monoid a) => a -> Bool
    monoidRightIdentity x = (x <> mempty) == x

    -- 1
    data Trivial = Trivial deriving (Eq, Show)
    
    instance Semigroup Trivial where
        Trivial <> Trivial = Trivial
    instance Monoid Trivial where
        mempty = Trivial
    instance Arbitrary Trivial where
        arbitrary = return Trivial

    type TrivialAssoc = Trivial -> Trivial -> Trivial -> Bool
    type TrivialId = Trivial -> Bool

    -- 2
    newtype Identity a = Identity a deriving (Eq, Show)

    instance Semigroup a => Semigroup (Identity a) where
        (Identity x) <> (Identity x') = Identity (x <> x')
    instance Monoid a => Monoid (Identity a) where
        mempty = Identity mempty
    instance Arbitrary a => Arbitrary (Identity a) where
        arbitrary = do
            x <- arbitrary
            return (Identity x)
    
    type IdentityAssoc = Identity String -> Identity String -> Identity String -> Bool
    type IdentityId = Identity String -> Bool

    -- 3
    data Two a b = Two a b deriving (Eq, Show)

    instance (Semigroup a, Semigroup b) => Semigroup (Two a b) where
        (Two x y) <> (Two x' y') = Two (x <> x') (y <> y')
    instance (Monoid a, Monoid b) => Monoid (Two a b) where
        mempty = Two mempty mempty
    instance (Arbitrary a, Arbitrary b) => Arbitrary (Two a b) where
        arbitrary = do
            x <- arbitrary
            y <- arbitrary
            return (Two x y)

    type TwoAssoc = Two String String -> Two String String -> Two String String -> Bool
    type TwoId = Two String String -> Bool

    -- 4
    newtype BoolConj = BoolConj Bool deriving (Eq, Show)

    instance Semigroup BoolConj where
        (BoolConj a) <> (BoolConj a') = BoolConj (a && a')
    instance Monoid BoolConj where
        mempty = BoolConj True
    instance Arbitrary BoolConj where
        arbitrary = frequency [(1, return $ BoolConj True), (1, return $ BoolConj False)]

    type BoolConjAssoc = BoolConj -> BoolConj -> BoolConj -> Bool
    type BoolConjId = BoolConj -> Bool

    -- 5
    newtype BoolDisj = BoolDisj Bool deriving (Eq, Show)

    instance Semigroup BoolDisj where
        (BoolDisj a) <> (BoolDisj a') = BoolDisj (a || a')
    instance Monoid BoolDisj where
        mempty = BoolDisj False
    instance Arbitrary BoolDisj where
        arbitrary = frequency [(1, return $ BoolDisj True), (1, return $ BoolDisj False)]

    type BoolDisjAssoc = BoolDisj -> BoolDisj -> BoolDisj -> Bool
    type BoolDisjId = BoolDisj -> Bool

    -- 6
    newtype Combine a b = Combine { unCombine :: a -> b }
    
    -- have to figure out Eq and Show instances for associativity to work out

    instance Semigroup b => Semigroup (Combine a b) where
        (Combine c) <> (Combine c') = Combine (c <> c')
    instance Monoid b => Monoid (Combine a b) where
        mempty = Combine $ \n -> mempty
    instance (CoArbitrary a, Arbitrary b) => Arbitrary (Combine a b) where
        arbitrary = do 
            f <- arbitrary
            return (Combine f)

    -- 7
    newtype Comp a = Comp { unComp :: a -> a }

    -- have to figure out Eq and Show instances for associativity to work out

    instance Semigroup a => Semigroup (Comp a) where
        Comp b <> Comp b' = Comp (b <> b')
    instance Monoid a => Monoid (Comp a) where
        mempty = Comp $ \n -> mempty
    instance (CoArbitrary a, Arbitrary a) => Arbitrary (Comp a) where
        arbitrary = do
            f <- arbitrary
            return (Comp f)

    -- 8
    newtype Mem s a = Mem { runMem :: s -> (a, s) }

    instance Semigroup a => Semigroup (Mem s a) where
        Mem f <> Mem g = Mem $ \v ->
            let (w, x) = g v
                (y, z) = f x in
                    (y <> w, z)
    instance Monoid a => Monoid (Mem s a) where
        mempty = Mem $ \s -> (mempty, s)

    
    -- Main function
    main :: IO()
    main = do
        putStrLn "Tests for Trivial..."
        quickCheck (monoidAssoc :: TrivialAssoc)
        quickCheck (monoidLeftIdentity :: TrivialId)
        quickCheck (monoidRightIdentity :: TrivialId)
        putStrLn "Tests for identity..."
        quickCheck (monoidAssoc :: IdentityAssoc)
        quickCheck (monoidLeftIdentity :: IdentityId)
        quickCheck (monoidRightIdentity :: IdentityId)
        putStrLn "Tests for Two..."
        quickCheck (monoidAssoc :: TwoAssoc)
        quickCheck (monoidLeftIdentity :: TwoId)
        quickCheck (monoidRightIdentity :: TwoId)
        putStrLn "Tests for BoolConj..."
        quickCheck (monoidAssoc :: BoolConjAssoc)
        quickCheck (monoidLeftIdentity :: BoolConjId)
        quickCheck (monoidRightIdentity :: BoolConjId)
        putStrLn "Tests for BoolDisj..."
        quickCheck (monoidAssoc :: BoolDisjAssoc)
        quickCheck (monoidLeftIdentity :: BoolDisjId)
        quickCheck (monoidRightIdentity :: BoolDisjId)

