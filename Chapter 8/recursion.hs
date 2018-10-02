sumNumbers :: (Eq a, Num a) => a -> a
sumNumbers n = go n 0 where
    go n answer
        | n == 0 = answer
        | otherwise = go (n - 1) (n + answer)

multipliedBy :: Integral a => a -> a -> a
multipliedBy a b = go a b a where
    go a b count
        | b == 0 = 0
        | b == 1 = count
        | otherwise = go a (b - 1) (a + count)