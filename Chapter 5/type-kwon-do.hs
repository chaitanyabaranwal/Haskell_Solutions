-- Question 1
f :: Int -> String
f = undefined

g :: String -> Char
g = undefined

h :: Int -> Char
h x = g (f x)

-- Question 2
data A
data B
data C

q :: A -> B
q = undefined

w :: B -> C
w = undefined

e :: A -> C
e x = w (q x)

-- Question 3
data X
data Y
data Z

xz :: X -> Z
xz = undefined

yz = Y -> Z
yz = undefined

xform :: (X, Y) -> (Z, Z)
xform (x, y) = (xz x, yz y)

munge :: (x -> y) -> (y -> (w, z)) -> x -> w
xToy :: x -> y
yTowz :: y -> (w, z)
xToy x = y
yTowz y = (w, _)
munge xToy yTowz x = fst (yTowz (xToy x))