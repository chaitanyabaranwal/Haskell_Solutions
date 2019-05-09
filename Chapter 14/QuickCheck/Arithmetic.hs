module Arithmetic where

    import Test.QuickCheck
    import Data.List (sort)

    -- Random generators for different types

    genInt :: Gen Int
    genInt = arbitrary

    genFloat :: Gen Float
    genFloat = arbitrary

    genString :: Gen String
    genString = arbitrary

    genIntList :: Gen [Int]
    genIntList = arbitrary

    nonZero = arbitrary `suchThat` (/= 0)
    genPositivePair :: Gen (Int, Int)
    genPositivePair = do
        a <- nonZero
        b <- nonZero
        return (a, b)

    genTwoIntLists :: Gen ([Int], [Int])
    genTwoIntLists = do
        a <- genIntList
        b <- genIntList
        return (a, b)

    genIntListOfLists :: Gen [[Int]]
    genIntListOfLists = do
        a <- genIntList
        return [a]

    positive = arbitrary `suchThat` (>= 0)
    genNumberListPair :: Gen (Int, [Int])
    genNumberListPair = do
        a <- positive
        b <- genIntList
        return (a, b)

    -- 1
    halfIdentity :: Fractional a => a -> a
    halfIdentity = (*2) . half where
        half x = x / 2
    
    propHalfIdentity :: Property
    propHalfIdentity = forAll genFloat (\x -> halfIdentity x == x)

    -- 2
    listOrdered :: (Ord a) => [a] -> Bool
    listOrdered xs =
        snd $ foldr go (Nothing, True) xs where 
            go _ status@(_, False) = status
            go y (Nothing, t) = (Just y, t)
            go y (Just x, t) = (Just y, x >= y)

    propListOrdered :: Property
    propListOrdered = forAll genIntList (\x -> listOrdered (sort x) == True)

    -- 3
    plusAssociative x y z = x + (y + z) == (x + y) + z
    propPlusAssociative = forAll genInt (\x y z -> plusAssociative x y z == True)

    -- 4
    multAssociative x y z = x * (y * z) == (x * y) * z
    propMultAssociative :: Property
    propMultAssociative = forAll genInt (\x y z -> multAssociative x y z == True)

    -- 5
    quotLaw x y = (quot x y)*y + (rem x y) == x
    divLaw x y = (div x y)*y + (mod x y) == x
    
    propDivision :: Property
    propDivision = forAll genPositivePair (\(x, y) -> quotLaw x y == True && divLaw x y == True)

    -- 6
    -- (^) is neither commutative nor associative

    -- 7
    propReverseList :: Property
    propReverseList = forAll genIntList (\x -> reverse (reverse x) == id x)

    -- 8
    dollarLaw a = id $ a == id a
    propDollarLaw :: Property
    propDollarLaw = forAll genInt (\x -> dollarLaw x == True)

    -- 9
    list1 = foldr (:)
    list2 = (++)
    list3 = foldr (++) []
    list4 = concat

    propTestFuncAEqual :: Property
    propTestFuncAEqual = forAll genTwoIntLists (\(x, y) -> list1 x y == list2 x y)

    propTestFuncBEqual :: Property
    propTestFuncBEqual = forAll genIntListOfLists (\x -> list3 x == list4 x)

    -- 10
    f n xs = length (take n xs) == n
    propTestTakeListLength :: Property
    propTestTakeListLength = forAll genNumberListPair (\(x, y) -> f x y == True)

    -- 11
    readShow x = (read (show x)) == x
    propTestReadShow :: Property
    propTestReadShow = forAll genInt (\x -> readShow x == True)


    -- Main function
    main :: IO()
    main = do
        putStrLn "Half identity..."
        quickCheck propHalfIdentity
        putStrLn "List ordered..."
        quickCheck propListOrdered
        putStrLn "Plus associative..."
        quickCheck propPlusAssociative
        putStrLn "Multiplication associative..."
        quickCheck propMultAssociative
        putStrLn "Division laws..."
        quickCheck propDivision
        putStrLn "Double reversing a list..."
        quickCheck propReverseList
        putStrLn "Testing the $ operator..."
        quickCheck propDollarLaw
        putStrLn "Testing equality of list functions..."
        quickCheck propTestFuncAEqual
        quickCheck propTestFuncBEqual
        putStrLn "Test if taking n elems from list makes a list of length n..."
        quickCheck propTestTakeListLength
        putStrLn "Test Read Show round trip..."
        quickCheck propTestReadShow
        