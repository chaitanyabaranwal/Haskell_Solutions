module Multiply where

    import Test.Hspec
    import Test.QuickCheck

    multiply :: (Eq a, Num a, Ord a) => a -> a -> a
    multiply _ 0 = 0
    multiply a b
        | b < 0 && a < 0 = multiply (negate a) (negate b)
        | b < 0 = negate (multiply a (negate b))
        | a < 0 = negate (multiply (negate a) b)
        | otherwise = go a b a where
            go a b result
                | b == 1 = result
                | otherwise = go a (b - 1) (result + a)

    main :: IO()
    main = hspec $ do
        describe "Multiply" $ do
            it "2 times 2 is 4" $ do
                multiply 2 2 `shouldBe` 4
            it "2 times -2 is -4" $ do
                multiply 2 (-2) `shouldBe` (-4)
            it "-2 times 2 is -4" $ do
                multiply (-2) 2 `shouldBe` (-4)
            it "-2 times -2 is 4" $ do
                multiply (-2) (-2) `shouldBe` 4
            it "x times 2 is equal to x" $ do
                property $ \x -> (multiply x 1) == (x :: Int)