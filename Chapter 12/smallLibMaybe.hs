-- 1

isJust :: Maybe a -> Bool
isJust Nothing = False
isJust (Just _) = True

isNothing :: Maybe a -> Bool
isNothing Nothing = True
isNothing (Just _ ) = False

-- 2

mayybee :: b -> (a -> b) -> Maybe a -> b
mayybee acc f Nothing = acc
mayybee acc f (Just x) = f x

-- 3

fromMaybe :: a -> Maybe a -> a
fromMaybe acc x = mayybee acc id x

-- 4

listToMaybe :: [a] -> Maybe a
listToMaybe [] = Nothing
listToMaybe x = Just (head x)

maybeToList :: Maybe a -> [a]
maybeToList Nothing = []
maybeToList (Just x) = [x]

-- 5

catMaybes :: [Maybe a] -> [a]
catMaybes [] = []
catMaybes (Nothing:xs) = catMaybes xs
catMaybes (Just x : xs) = [x] ++ catMaybes xs

-- 6

flipMaybe :: Eq a => [Maybe a] -> Maybe [a]
flipMaybe list
    | elem Nothing list = Nothing
    | otherwise = Just (catMaybes list)