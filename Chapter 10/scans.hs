-- Normal fibs infinite
fibs = 1 : scanl (+) 1 fibs

-- Get particular fibonacci number
fibsN n = fibs !! n

-- Return fibs for first 20
fibs20 =  take 20 fibs

-- Fibs less than 100
fibsLess100 = takeWhile (<100) fibs

-- Factorial
factorial = scanl (*) 1 [2..]
factorialN n = take n factorial