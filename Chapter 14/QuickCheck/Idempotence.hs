module Idempotence where

    import Test.QuickCheck
    import Data.Char (toUpper)
    import Data.List (sort)

    -- Random generator
    genString :: Gen String
    genString = arbitrary

    genIntList :: Gen [Int]
    genIntList = arbitrary

    -- Define functions
    twice f = f . f
    fourTimes = twice . twice

    capitalizeWord :: String -> String
    capitalizeWord "" = ""
    capitalizeWord (x:xs) = ((toUpper x):xs)
    f x = capitalizeWord x == twice capitalizeWord x && 
            twice capitalizeWord x == fourTimes capitalizeWord x
    g x = sort x == twice sort x && 
            twice sort x == fourTimes sort x

    -- Tests
    prop_f :: Property
    prop_f = forAll genString (\x -> f x == True)

    prop_g :: Property
    prop_g = forAll genIntList (\x -> g x == True)

    -- Main function
    main :: IO()
    main = do
        putStrLn "Checking capitalizeWord..."
        quickCheck prop_f
        putStrLn "Checking list sort..."
        quickCheck prop_g