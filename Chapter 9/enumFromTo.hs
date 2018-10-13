eftBool :: Bool -> Bool -> [Bool]
eftBool bool1 bool2
    | bool1 > bool2 = []
    | bool1 == bool2 = [bool1]
    | otherwise = [bool1] ++ (eftBool (succ bool1) bool2)

eftOrd :: Ordering -> Ordering -> [Ordering]
eftOrd ord1 ord2
    | ord1 > ord2 = []
    | ord2 == ord1 = [ord1]
    | otherwise = [ord1] ++ (eftOrd (succ ord1) ord2)

eftInt :: Int -> Int -> [Int]
eftInt int1 int2
    | int1 > int2 = []
    | otherwise = [int1] ++ (eftInt (succ int1) int2)

eftChar :: Char -> Char -> [Char]
eftChar char1 char2
    | char1 > char2 = []
    | otherwise = [char1] ++ (eftChar (succ char1) char2)
