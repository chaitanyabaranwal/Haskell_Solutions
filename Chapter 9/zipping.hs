--1
myZip :: [a] -> [b] -> [(a, b)]
myZip _ [] = []
myZip [] _ = []
myZip list1 list2 = [(head list1, head list2)] ++ (myZip (tail list1) (tail list2))

--2
myZipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
myZipWith f [] _ = []
myZipWith f _ [] = []
myZipWith f list1 list2 = [f (head list1) (head list2)] ++ (myZipWith f (tail list1) (tail list2))

--3
myZip2 :: [a] -> [b] -> [(a, b)]
f a b = (a, b)
myZip2 list1 list2 = myZipWith f list1 list2