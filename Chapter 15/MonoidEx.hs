module MonoidEx where

    import Test.QuickCheck
    import Test.QuickCheck.Gen (oneof)
    import Data.Monoid
    import SemigroupEx

    -- Monoidal Laws 
    monoidLeftIdentity :: (Eq a, Monoid a) => a -> Bool
    monoidLeftIdentity a = (mempty <> a) == a

    monoidRightIdentity :: (Eq a, Monoid a) => a -> Bool
    monoidRightIdentity a = (a <> mempty) == a

    -- 1
    instance Monoid Trivial where
        mempty = Trivial
    type TrivialId = Trivial -> Bool
    
    -- 2
    instance Monoid a => Monoid (Identity a) where
        mempty = Identity mempty
    type IdentityId = Identity String -> Bool

    -- 3
    instance (Monoid a, Monoid b) => Monoid (Two a b) where
        mempty = Two mempty mempty
    type TwoId = Two String String -> Bool

    -- 4
    instance Monoid BoolConj where
        mempty = BoolConj True
    type BoolConjId = BoolConj -> Bool

    -- 5
    instance Monoid BoolDisj where
        mempty = BoolDisj False
    type BoolDisjId = BoolDisj -> Bool

    -- 6
    -- Have to figure out how to check
    instance Monoid (Combine a b) where
        mempty = Combine $ \n -> n

    -- 7
    -- Have to figure out how to check
    instance Monoid (Comp a) where
        mempty = Comp $ \n -> n

    -- 8
    newtype Mem s a = Mem { runMem :: s -> (a, s) }
    instance Semigroup a => Semigroup (Mem s a) where
        Mem f <> Mem g = Mem (\x -> 
            let (a, b) = g x
                (c, d) = f b
            in  (a <> c, d))
    instance (Semigroup a, Monoid a) => Monoid (Mem s a) where
        mempty = Mem (\x -> (mempty, x))

    -- Main function
    main :: IO()
    main = do
        putStrLn "Testing Trivial..."
        quickCheck (monoidLeftIdentity :: TrivialId)
        quickCheck (monoidRightIdentity :: TrivialId)
        putStrLn "Testing Identity..."
        quickCheck (monoidLeftIdentity :: IdentityId)
        quickCheck(monoidRightIdentity :: IdentityId)
        putStrLn "Testing Two..."
        quickCheck (monoidLeftIdentity :: TwoId)
        quickCheck(monoidRightIdentity :: TwoId)
        putStrLn "Testing BoolConj..."
        quickCheck (monoidLeftIdentity :: BoolConjId)
        quickCheck(monoidRightIdentity :: BoolConjId)
        putStrLn "Testing BoolDisj..."
        quickCheck (monoidLeftIdentity :: BoolDisjId)
        quickCheck(monoidRightIdentity :: BoolDisjId)
