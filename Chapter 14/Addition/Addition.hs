module Addition where

    import Test.Hspec
    import Test.QuickCheck

    -- Function to handle division
    divideBy :: Integral a => a -> a -> (a, a)
    divideBy a b = go a b 0 where
        go n d q
            | (n < d) = (q, mod n d)
            | otherwise = go (n - d) d (q + 1)

    -- Function to handle multiplication
    multiply :: Integral a => a -> a -> a
    multiply a b = go 0 a b where
        go ans termA termB
            | termB <= 0 = ans
            | otherwise = go (ans + termA) termA (termB - 1)

    main :: IO()
    main = hspec $ do
        describe "Addition" $ do
            it "1 + 1 is greater than 1" $ do
                (1 + 1) > 1 `shouldBe` True
            it "2 + 2 is equal to 4" $ do
                (2 + 2) `shouldBe` 4
            it "x + 1 is always greater than x" $ do
                property $ \x -> x + 1 > (x :: Int)
        describe "Division" $ do
            it "10 / 5 is equal to (2, 0)" $ do
                divideBy 10 5 `shouldBe` (2, 0)
            it "23 / 4 is equal to (5, 3)" $ do
                divideBy 23 4 `shouldBe` (5, 3)
        describe "Multiplication" $ do
            it "7 * 3 is equal to 21" $ do
                multiply 7 3 `shouldBe` 21
            it "11 * 12 is equal to 132" $ do
                multiply 11 12 `shouldBe` 132
