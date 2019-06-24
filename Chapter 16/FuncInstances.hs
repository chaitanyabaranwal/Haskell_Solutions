module FuncInstances where

    import Test.QuickCheck
    import Test.QuickCheck.Function

    -- Functor laws
    functorId :: (Eq (f a), Functor f) => f a -> Bool
    functorId f = fmap id f == f

    functorCompose :: (Eq (f c), Functor f) => Fun a b -> Fun b c -> f a -> Bool
    functorCompose (Fun _ f) (Fun _ g) x = (fmap g (fmap f x)) == (fmap (g . f) x)

    -- 1
    newtype Identity a = Identity a deriving (Eq, Show)
    instance Functor Identity where
        fmap f (Identity a) = Identity (f a)
    instance Arbitrary a => Arbitrary (Identity a) where
        arbitrary = do
            a <- arbitrary
            return (Identity a)
    type IdentityId = Identity Int -> Bool
    type IdentityCompose = Fun Int Int -> Fun Int Int -> Identity Int -> Bool

    -- 2
    data Pair a = Pair a a deriving (Eq, Show)
    instance Functor Pair where
        fmap f (Pair a b) = Pair (f a) (f b)
    instance Arbitrary a => Arbitrary (Pair a) where
        arbitrary = do
            a <- arbitrary
            b <- arbitrary
            return (Pair a b)
    type PairId = Pair Int -> Bool
    type PairCompose = Fun Int Int -> Fun Int Int -> Pair Int -> Bool

    -- 3
    data Two a b = Two a b deriving (Eq, Show)
    instance Functor (Two a) where
        fmap f (Two a b) = Two a (f b)
    instance (Arbitrary a, Arbitrary b) => Arbitrary (Two a b) where
        arbitrary = do
            a <- arbitrary
            b <- arbitrary
            return (Two a b)
    type TwoId = Two Int Int -> Bool
    type TwoCompose = Fun Int Int -> Fun Int Int -> Two Int Int -> Bool

    -- 4
    data Three a b c = Three a b c deriving (Eq, Show)
    instance Functor (Three a b) where
        fmap f (Three a b c) = Three a b (f c)
    instance (Arbitrary a, Arbitrary b, Arbitrary c) => Arbitrary (Three a b c) where
        arbitrary = do
            a <- arbitrary
            b <- arbitrary
            c <- arbitrary
            return (Three a b c)
    type ThreeId = Three Int Int Int -> Bool
    type ThreeCompose = Fun Int Int -> Fun Int Int -> Three Int Int Int -> Bool
    
    -- 5
    data Three' a b = Three' a b b deriving (Eq, Show)
    instance Functor (Three' a) where
        fmap f (Three' a b c) = Three' a (f b) (f c)
    instance (Arbitrary a, Arbitrary b) => Arbitrary (Three' a b) where
        arbitrary = do
            a <- arbitrary
            b <- arbitrary
            c <- arbitrary
            return (Three' a b c)
    type Three'Id = Three' Int Int -> Bool
    type Three'Compose = Fun Int Int -> Fun Int Int -> Three' Int Int -> Bool

    -- 6
    data Four a b c d = Four a b c d deriving (Eq, Show)
    instance Functor (Four a b c) where
        fmap f (Four a b c d) = Four a b c (f d)
    instance (Arbitrary a, Arbitrary b, Arbitrary c, Arbitrary d) => Arbitrary (Four a b c d) where
        arbitrary = do
            a <- arbitrary
            b <- arbitrary
            c <- arbitrary
            d <- arbitrary
            return (Four a b c d)
    type FourId = Four Int Int Int Int -> Bool
    type FourCompose = Fun Int Int -> Fun Int Int -> Four Int Int Int Int -> Bool

    -- 7
    data Four' a b = Four' a a a b deriving (Eq, Show)
    instance Functor (Four' a) where
        fmap f (Four' a b c d) = Four' a b c (f d)
    instance (Arbitrary a, Arbitrary b) => Arbitrary (Four' a b) where
        arbitrary = do
            a <- arbitrary
            b <- arbitrary
            c <- arbitrary
            d <- arbitrary
            return (Four' a b c d)
    type Four'Id = Four' Int Int -> Bool
    type Four'Compose = Fun Int Int -> Fun Int Int -> Four' Int Int -> Bool

    -- Main function
    main :: IO()
    main = do
        putStrLn "Testing Identity..."
        quickCheck (functorId :: IdentityId)
        quickCheck (functorCompose :: IdentityCompose)
        putStrLn "Testing Pair..."
        quickCheck (functorId :: PairId)
        quickCheck (functorCompose :: PairCompose)
        putStrLn "Testing Two..."
        quickCheck (functorId :: TwoId)
        quickCheck (functorCompose :: TwoCompose)
        putStrLn "Testing Three..."
        quickCheck (functorId :: ThreeId)
        quickCheck (functorCompose :: ThreeCompose)
        putStrLn "Testing Three'..."
        quickCheck (functorId :: Three'Id)
        quickCheck (functorCompose :: Three'Compose)
        putStrLn "Testing Four..."
        quickCheck (functorId :: FourId)
        quickCheck (functorCompose :: FourCompose)
        putStrLn "Testing Four'..."
        quickCheck (functorId :: Four'Id)
        quickCheck (functorCompose :: Four'Compose)