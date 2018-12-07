-- recursive datatype, 'succ' means successive
data Nat = Zero | Succ Nat deriving (Eq, Show)

-- convert 'Nat' datatype to integer
natToInteger :: Nat -> Integer
natToInteger number = go number 0 where
    go Zero count = count
    go (Succ x) count = go x (count + 1)

-- convert integer to 'Nat'
integerToNat :: Integer -> Maybe Nat
integerToNat number
    | number < 0 = Nothing
    | otherwise = Just (go number) where
        go number
            | number == 0 = Zero
            | otherwise = Succ (go (number - 1))
