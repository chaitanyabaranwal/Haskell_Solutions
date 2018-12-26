module Main where

    import qualified Data.Map as M
    import Morse
    import Test.QuickCheck

    allowedChars :: [Char]
    allowedChars = M.keys letterToMorse

    allowedMorse :: [Morse]
    allowedMorse = M.elems letterToMorse

    charGen :: Gen Char
    charGen = elements allowedChars

    morseGen :: Gen Morse
    morseGen = elements allowedMorse

    prop_ThereAndBackAgain :: Property
    prop_ThereAndBackAgain = forAll charGen (\c -> (charToMorse c >>= morseToChar) == Just c)

    main :: IO()
    main = quickCheck prop_ThereAndBackAgain
