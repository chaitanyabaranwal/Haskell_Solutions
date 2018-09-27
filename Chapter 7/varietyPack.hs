-- 1

k (x, y) = x
k1 = k ((4-1), 10)
k2 = k ("three", (1 + 2))
k3 = k (3, True)

-- k :: (a, b) -> a
-- k1 :: Num a => a
-- k2 :: [Char]
-- k3 :: Num a => a

-- 2

f :: (a, b, c) -> (d, e, f) -> ((a, d), (c, f))
f (a, b, c) (d, e, f) = ((a, d), (c, f))