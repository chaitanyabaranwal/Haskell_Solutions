module Spec where

    import Main
    import Test.Hspec

    word = "test"
    puzzle = Puzzle word (replicate (length word) Nothing) [] 0
    puzzle2 = Puzzle word (replicate (length word) Nothing) ['a'] 1

    mainTest :: IO()
    mainTest = hspec $ do
        describe "Testing fillInChar..." $ do
            it "t is in word" $ do
                fillInChar puzzle 't' `shouldBe` (Puzzle word [Just 't', Nothing, Nothing, Just 't'] ['t'] 0)
            it "a not in word" $ do
                fillInChar puzzle 'a' `shouldBe` (Puzzle word (replicate (length word) Nothing) ['a'] 0)
        describe "Testing handleGuess..." $ do
            it "letter correctly guessed" $ do
                puzzle3 <- handleGuess puzzle2 't'
                puzzle3 `shouldBe` (Puzzle word [Just 't', Nothing, Nothing, Just 't'] ['t', 'a'] 1)
            it "letter already guessed" $ do
                puzzle3 <- handleGuess puzzle2 'a'
                puzzle3 `shouldBe` puzzle2
            it "letter incorrectly guessed" $ do
                puzzle3 <- handleGuess puzzle2 'b'
                puzzle3 `shouldBe` (Puzzle word (replicate (length word) Nothing) ['b', 'a'] 2)


