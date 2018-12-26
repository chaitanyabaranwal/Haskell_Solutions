module QuickCheckTests where

    import Test.QuickCheck
    import qualified Idempotence as I
    import Data.List (sort)

    --Generator functions

    floatGen :: Gen Float
    floatGen = arbitrary

    stringGen :: Gen String
    stringGen = arbitrary

    listGen :: Gen [Float]
    listGen = arbitrary

    stringListGen :: Gen [String]
    stringListGen = arbitrary

    threeIntsGen :: Gen (Int, Int, Int)
    threeIntsGen = do
        a <- arbitrary
        b <- arbitrary
        c <- arbitrary
        return (a, b, c)

    twoIntsGen :: Gen (Int, Int)
    twoIntsGen = do
        a <- arbitrary
        b <- arbitrary
        return (a, b)

    nonZeroArbitrary = arbitrary `suchThat` (/= 0)
    twoNonZerosGen :: Gen (Int, Int)
    twoNonZerosGen = do
        a <- nonZeroArbitrary
        b <- nonZeroArbitrary
        return (a, b)

    nonNegativeArbitrary = arbitrary `suchThat` (>= 0)
    intStringGen :: Gen (Int, String)
    intStringGen = do
        n <- nonNegativeArbitrary
        xs <- arbitrary
        return (n, xs)

    --1
    halfIdentity :: Fractional a => a -> a
    halfIdentity = (*2) . half where
        half x = x / 2

    prop_halfIdentity :: Property
    prop_halfIdentity = forAll floatGen (\x -> halfIdentity x == x) 

    --2
    listOrdered :: (Ord a) => [a] -> Bool
    listOrdered xs = snd $ foldr go (Nothing, True) xs where
        go _ status@(_, False) = status
        go y (Nothing, True) = (Just y, True)
        go y (Just x, True) = (Just y, x >= y)

    prop_listOrdered :: Property
    prop_listOrdered = forAll listGen (\xs -> listOrdered (sort xs) == True)

    --3
    prop_plusAssociative :: Property
    prop_plusAssociative = forAll threeIntsGen (\(a, b, c) -> (a + b) + c == a + (b + c))
    prop_plusCommutative :: Property
    prop_plusCommutative = forAll twoIntsGen (\(a, b) -> a + b == b + a)

    --4 is similar

    --5
    prop_quotRem :: Property
    prop_quotRem = forAll twoNonZerosGen (\(x ,y) -> (quot x y)*y + (rem x y) == x) 
    prop_divMod :: Property
    prop_divMod = forAll twoNonZerosGen (\(x, y) -> (div x y)*y + (mod x y) == x)

    --7
    prop_twiceReverse :: Property
    prop_twiceReverse = forAll listGen (\xs -> (reverse . reverse) xs == id xs)

    --8
    prop_dollarFunc :: Property
    prop_dollarFunc = forAll floatGen (\x -> id $ x == id x)
    prop_composition :: Property
    prop_composition = forAll floatGen (\x -> (id . id) x == id (id x))

    --9
    prop_foldOne :: Property
    prop_foldOne = forAll listGen (\xs -> foldr (:) [] xs == (++) [] xs)
    prop_foldTwo :: Property
    prop_foldTwo = forAll stringListGen (\xs -> foldr (++) [] xs == concat xs)

    --10
    prop_lengthTake :: Property
    prop_lengthTake = forAll intStringGen (\(n, xs) -> length (take n xs) == n)

    --11
    prop_readShow :: Property
    prop_readShow = forAll stringGen (\xs -> read (show xs) == xs)

    -- Failure
    square x = x * x
    squareIdentity = square . sqrt

    prop_square :: Property
    prop_square = forAll floatGen (\x -> x * x == square x)
    prop_squareIdentity :: Property
    prop_squareIdentity = forAll floatGen (\x -> squareIdentity x == x)



    -- Perform tests
    main :: IO()
    main = do
        putStrLn "Testing halfIdentity..."
        quickCheck prop_halfIdentity
        putStrLn "Testing listOrdered..."
        quickCheck prop_listOrdered
        putStrLn "Testing plusAssociative..."
        quickCheck prop_plusAssociative
        putStrLn "Testing plusCommutative..."
        quickCheck prop_plusCommutative
        putStrLn "Testing quotRem and divMod..."
        quickCheck prop_quotRem
        quickCheck prop_divMod
        putStrLn "Testing twiceReverse..."
        quickCheck prop_twiceReverse
        putStrLn "Testing dollarFunc..."
        quickCheck prop_dollarFunc
        putStrLn "Testing composition..."
        quickCheck prop_composition
        putStrLn "Testing foldOne..."
        quickCheck prop_foldOne
        putStrLn "Testing foldTwo..."
        quickCheck prop_foldTwo
        putStrLn "Testing lengthTake..."
        quickCheck prop_lengthTake
        putStrLn "Testing readShow..."
        quickCheck prop_readShow
        putStrLn "Testing square..."
        quickCheck prop_square
        putStrLn "Testing squareIdentity..."
        quickCheck prop_squareIdentity
        putStrLn "Testing capitalizeWord..."
        quickCheck I.prop_capitalize
        putStrLn "Testing sort..."
        quickCheck I.prop_sort