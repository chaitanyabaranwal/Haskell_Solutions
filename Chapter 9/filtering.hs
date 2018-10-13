-- 1
list = filter (\x -> rem x 3 == 0) [1..30]

-- 2
listLen = length $ filter (\x -> rem x 3 == 0) [1..30]

-- 3
myFilter sentence = [x | x <- words sentence, elem x ["the", "a", "an"] == False]