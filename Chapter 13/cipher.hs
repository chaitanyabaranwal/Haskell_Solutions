import Data.Char

-- Caesar cipher

caesar :: String -> Int -> String
caesar "" _ = ""
caesar (x:rem_string) key = [chr $ (mod (ord x - 97 + key) 26) + 97] ++ caesar rem_string key

uncaesar :: String -> Int -> String
uncaesar "" _ = ""
uncaesar (x:rem_string) key = [chr $ (mod (ord x - 97 + (negate key)) 26) + 97] ++ uncaesar rem_string key

caesarConvert :: IO()
caesarConvert = do
    putStrLn "Enter string to convert: "
    string <- getLine
    putStrLn "Enter key: "
    key <- getLine
    putStrLn $ "Encrypted string is: " ++ (caesar string (read key :: Int))

-- Vignere cipher

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

vignereConvert :: IO()
vignereConvert = do
    putStrLn "Enter string to convert: "
    string <- getLine
    putStrLn "Enter key: "
    key <- getLine
    putStrLn $ "Encrypted string is: " ++ (vignere string key)