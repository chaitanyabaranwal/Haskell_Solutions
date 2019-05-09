module Main where

    import qualified Data.Map as M
    import Morse
    import Test.QuickCheck

    type Morse = String

    allowedChar :: [Char]
    allowedChar = M.keys letterToMorse

    allowedMorse :: [Morse]
    allowedMorse = M.elems letterToMorse

    charGen :: Gen Char
    charGen = elements allowedChar

    morseGen :: Gen Morse
    morseGen = elements allowedMorse

    prop_thereAndBackAgain :: Property
    prop_thereAndBackAgain = forAll charGen (\c -> ((charToMorse c) >>= morseToChar) == Just c)

    main :: IO()
    main = quickCheck prop_thereAndBackAgain
        