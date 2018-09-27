-- 1

tensDigit x = (fst (divMod x 10)) `mod` 10 -- The type is the same
hundredsDigit x = (fst (divMod x 100)) `mod` 10
hundredsDigit2 x = d where
    xByHundred = x `div` 100
    d = xByHundred `mod` 10

-- 2

foldBool :: a -> a -> Bool -> a
foldBool x y z = if z == True then x else y
foldBool2 :: a -> a -> Bool -> a
foldBool2 x y z
    | z == True = x
    | otherwise = y
-- 3

g :: (a -> b) -> (a, c) -> (b, c)
g f (a, c) = (f a, c)