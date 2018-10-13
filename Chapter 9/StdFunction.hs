module StdFunction where

    myOr :: [Bool] -> Bool
    myOr [] = True
    myOr (x:xs) = x || myOr xs

    myAny :: (a -> Bool) -> [a] -> Bool
    myAny _ [] = False
    myAny f (x:xs) = (f x) || (myAny f xs)

    myElem :: Eq a => a -> [a] -> Bool
    myElem _ [] = False
    myElem a (x:xs) = (a == x) || (myElem a xs)

    myElem2 :: Eq a => a -> [a] -> Bool
    myElem2 _ [] = False
    myElem2 a (x:xs) = any (== a) (x:xs)

    myReverse :: [a] -> [a]
    myReverse [] = []
    myReverse list = myReverse (tail list) ++ [head list]

    squish :: [[a]] -> [a]
    squish [] = []
    squish (x:xs) = x ++ squish xs

    squishMap :: (a -> [b]) -> [a] -> [b]
    squishMap _ [] = []
    squishMap f (x:xs) = f x ++ squishMap f xs

    squishAgain :: [[a]] -> [a]
    squishAgain = squishMap id

    myMaximumBy :: (a -> a -> Ordering) -> [a] -> a
    myMaximumBy _ [] = undefined
    myMaximumBy f (x:xs) = go f xs x where
        go f list max
            | length list == 1 = max
            | f (head list) max == GT = go f (tail list) (head list)
            | otherwise = go f (tail list) max

    myMinimumBy :: (a -> a -> Ordering) -> [a] -> a
    myMinimumBy _ [] = undefined
    myMinimumBy f (x:xs) = go f xs x where
        go f list min
            | length list == 1 = min
            | f (head list) min == LT = go f (tail list) (head list)
            | otherwise = go f (tail list) min

    myMaximum :: Ord a => [a] -> a
    myMaximum = myMaximumBy compare

    myMinimum :: Ord a => [a] -> a
    myMinimum = myMinimumBy compare