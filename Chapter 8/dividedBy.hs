-- data DividedByResult a = Result a | DividedByZero deriving Show

-- dividedBy :: Integral a => a -> a -> DividedByResult a
-- dividedBy _ 0 = DividedByZero
-- dividedBy num denom = Result (go num denom 0) where
--     go n d q
--         | (n < 0) && (d < 0) = go (negate n) (negate d) q
--         | (n < 0) = negate (go (negate n) d q)
--         | (d < 0) = negate (go n (negate d) q)
--         | n < d = q
--         | otherwise = go (n - d) d (q + 1)

data DividedByResult a = Result a | DividedByZero deriving Show

dividedBy :: Integral a => a -> a -> DividedByResult a
dividedBy num denom
    | denom == 0 = DividedByZero
    | signum num == signum denom = Result r
    | otherwise = Result (-r)
    where
        r = go (abs num) (abs denom) 0
        go n d q
            | n < d = q
            | otherwise = go (n - d) d (1 + q)