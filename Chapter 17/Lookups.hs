{- In the following exercises you will need to use the following terms
to make the expressions type-check:
    1. pure
    2. (<$>)
        -- or fmap
    3. (<*>)
Make the following expressions type-check. -}

import Data.List (elemIndex)

-- 1
-- added = (+3) (lookup 3 $ zip [1, 2, 3] [4, 5, 6])
added :: Maybe Integer
added = (+3) <$> (lookup 3 $ zip [1, 2, 3] [4, 5, 6])

-- 2
{- 
    y :: Maybe Integer
    y = lookup 3 $ zip [1, 2, 3] [4, 5, 6]

    z :: Maybe Integer
    z = lookup 2 $ zip [1, 2, 3] [4, 5, 6]

    tupled :: Maybe (Integer, Integer)
    tupled = (,) y z 
-}

y :: Maybe Integer
y = lookup 3 $ zip [1, 2, 3] [4, 5, 6]

z :: Maybe Integer
z = lookup 2 $ zip [1, 2, 3] [4, 5, 6]

tupled :: Maybe (Integer, Integer)
tupled = (,) <$> y <*> z

-- 3
{-
    x :: Maybe Int
    x = elemIndex 3 [1, 2, 3, 4, 5]
    
    y :: Maybe Int
    y = elemIndex 4 [1, 2, 3, 4, 5]

    max' :: Int -> Int -> Int
    max' = max

    maxed :: Maybe Int
    maxed = max' x y
-}

x :: Maybe Int
x = elemIndex 3 [1, 2, 3, 4, 5]

y' :: Maybe Int
y' = elemIndex 4 [1, 2, 3, 4, 5]

max' :: Int -> Int -> Int
max' = max

maxed :: Maybe Int
maxed = max' <$> x <*> y'

-- 4
{- 
    xs = [1, 2, 3]
    ys = [4, 5, 6]

    x :: Maybe Integer
    x = lookup 3 $ zip xs ys

    y :: Maybe Integer
    y = lookup 2 $ zip xs ys

    summed :: Maybe Integer
    summed = sum $ (,) x y
-}

xs = [1, 2, 3]
ys = [4, 5, 6]

x' :: Maybe Integer
x' = lookup 3 $ zip xs ys

y'' :: Maybe Integer
y'' = lookup 2 $ zip xs ys

summed :: Maybe Integer
summed = fmap sum $ (,) <$> x' <*> y''