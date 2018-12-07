-- 1

myIterate :: (a -> a) -> a -> [a]
myIterate f x = [x] ++ myIterate f (f x)

-- 2

myUnfoldr :: (b -> Maybe (a, b)) -> b -> [a]
myUnfoldr f x = case (f x) of
    Nothing -> []
    Just (x, y) -> [x] ++ myUnfoldr f y

-- 3

betterIterate :: (a -> a) -> a -> [a]
betterIterate f = myUnfoldr (\b -> Just (b, f b))