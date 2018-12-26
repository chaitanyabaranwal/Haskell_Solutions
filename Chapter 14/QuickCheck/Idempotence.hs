module Idempotence where

    import Test.QuickCheck
    import Data.List (sort)
    import Data.Char (toUpper)

    twice f = f . f
    fourTimes = twice . twice
    capitalizeWord :: String -> String
    capitalizeWord "" = ""
    capitalizeWord (x:xs) = (toUpper x : xs)

    fOne xs = (capitalizeWord xs == twice capitalizeWord xs) && (capitalizeWord xs == fourTimes capitalizeWord xs)
    fTwo xs = (sort xs == twice sort xs) && (sort xs == fourTimes sort xs)

    -- Generators
    stringGen :: Gen String
    stringGen = arbitrary

    prop_capitalize :: Property
    prop_capitalize = forAll stringGen (\xs -> fOne xs)

    prop_sort :: Property
    prop_sort = forAll stringGen (\xs -> fTwo xs)