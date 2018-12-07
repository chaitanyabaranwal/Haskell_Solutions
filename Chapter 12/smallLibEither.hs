-- 1

lefts' :: [Either a b] -> [a]
lefts' = foldr left [] where
    left (Left x) xs = [x] ++ xs
    left (Right _) xs = xs

-- More concise solution: lefts' list = [value | (Left value) <- list]

-- 2

rights' :: [Either a b] -> [b]
rights' = foldr right [] where
    right (Left _) xs = xs
    right (Right x) xs = [x] ++ xs

-- More concise solution: lefts' list = [value | (Right value) <- list]

-- 3

partitionEithers' :: [Either a b] -> ([a], [b])
partitionEithers' list = (lefts' list, rights' list)

-- 4

eitherMaybe' :: (b -> c) -> Either a b -> Maybe c
eitherMaybe' f x = case x of
    (Right x) -> Just (f x)
    (Left _) -> Nothing

-- 5

either' :: (a -> c) -> (b -> c) -> Either a b -> c
either' f g x = case x of
    (Right x) -> g x
    (Left x) -> f x

-- 6

eitherMaybe'' :: (b -> c) -> Either a b -> Maybe c
eitherMaybe'' f = either' (\a -> Nothing) (\b -> Just (f b))