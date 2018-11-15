import Data.Char

keygen :: String -> String -> String
keygen (x:remString) key = go (x:remString) key 0 where
    go "" _ _ = ""
    go (x:remString) key counter
        | x == ' ' = " " ++ (go remString key counter)
        | otherwise = [key !! (mod counter (length key))] ++ (go remString key (counter + 1))

convertString :: String -> String -> String
convertString string key = go string key where
    go "" _ = ""
    go (x:remString) (y:remKey)
        | x == ' ' = " " ++ (go remString remKey)
        | otherwise = [chr $ (mod (ord x - 65 + (ord y - 65)) 26) + 65] ++ (go remString remKey)

vignere :: String -> String -> String
vignere string key = convertString string (keygen string key)