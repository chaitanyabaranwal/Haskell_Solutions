-- 1
myOr :: [Bool] -> Bool
myOr = foldr (||) False

-- 2
myAny :: (a -> Bool) -> [a] -> Bool
myAny f = foldr (\a b -> if (f a) == True then True else b) False
-- Alternate version: myAny f = foldr ((||) . f) False

-- 3
myElem :: Eq a => a -> [a] -> Bool
myElem elem = any (== elem)

myElem2 :: Eq a => a -> [a] -> Bool
myElem2 elem = foldr ((||) . (== elem)) False
-- Alternate function: (\a b -> if (a == elem) then True else b)

-- 4
myReverse :: [a] -> [a]
myReverse = foldl (flip (:)) []

-- 5
myMap :: (a -> b) -> [a] -> [b]
myMap f = foldr ((:) . f) []

-- 6
myFilter :: (a -> Bool) -> [a] -> [a]
myFilter f = foldr (\a b -> if (f a) == True then ([a] ++ b) else b) []

-- 7
squish :: [[a]] -> [a]
squish = foldr (++) []

-- 8
squishMap :: (a -> [b]) -> [a] -> [b]
squishMap f = foldr ((++) . f) []

-- 9
squishAgain :: [[a]] -> [a]
squishAgain = squishMap id

-- 10
myMaximumBy :: (a -> a -> Ordering) -> [a] -> a
myMaximumBy f (x:xs) = foldr (\a b -> if (f a b) == GT then a else b) (last xs) (x:xs)

-- 11
myMinimumBy :: (a -> a -> Ordering) -> [a] -> a
myMinimumBy f (x:xs) = foldr (\a b -> if (f a b) == LT then a else b) (last xs) (x:xs)