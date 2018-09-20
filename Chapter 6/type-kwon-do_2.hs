-- 1

chk :: Eq b => (a -> b) -> a -> b -> Bool
chk aTob a b = (aTob a) == b

-- 2

arith :: Num b => (a -> b) -> Integer -> a -> b
arith aTob f a = (aTob a) + (fromInteger f)