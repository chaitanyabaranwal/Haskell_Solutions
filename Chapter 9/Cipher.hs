module Cipher where

    import Data.Char

    caesar :: String -> Int -> String
    caesar "" _ = ""
    caesar (x:rem_string) key = [chr $ (mod (ord x - 97 + key) 26) + 97] ++ caesar rem_string key

    uncaesar :: String -> Int -> String
    uncaesar "" _ = ""
    uncaesar (x:rem_string) key = [chr $ (mod (ord x - 97 + (negate key)) 26) + 97] ++ uncaesar rem_string key