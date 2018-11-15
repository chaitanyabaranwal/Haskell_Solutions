data BinaryTree a = Leaf | Node (BinaryTree a) a (BinaryTree a) deriving (Eq, Ord, Show)
insert' :: Ord a => a -> BinaryTree a -> BinaryTree a
insert' b Leaf = Node Leaf b Leaf
insert' b (Node left a right)
    | b == a = Node left a right
    | b < a = Node (insert' b left) a right
    | b > a = Node left a (insert' b right)

-- Map for binary tree

mapTree :: (a -> b) -> BinaryTree a -> BinaryTree b
mapTree _ Leaf = Leaf
mapTree f (Node left a right) = Node (mapTree f left) (f a) (mapTree f right)

testTree :: BinaryTree Integer
testTree = Node (Node Leaf 3 Leaf) 1 (Node Leaf 4 Leaf)
mapExpected = Node (Node Leaf 4 Leaf) 2 (Node Leaf 5 Leaf)

mapOkay = 
    if (mapTree (+1) testTree) == mapExpected
        then print "Yep okay!"
        else error "Unexpected :("

-- Convert binary trees into lists

preOrder :: BinaryTree a -> [a]
preOrder Leaf = []
preOrder (Node left a right) = [a] ++ (preOrder left) ++ (preOrder right)

inOrder :: BinaryTree a -> [a]
inOrder Leaf = []
inOrder (Node left a right) = (inOrder left) ++ [a] ++ (inOrder right)

postOrder :: BinaryTree a -> [a]
postOrder Leaf = []
postOrder (Node left a right) = (postOrder left) ++ (postOrder right) ++ [a]

testTree' = Node (Node Leaf 1 Leaf) 2 (Node Leaf 3 Leaf)

testPreOrder :: IO()
testPreOrder = 
    if preOrder testTree' == [2, 1, 3]
        then putStrLn "Pre-order cool!"
        else putStrLn "Something is wrong."

testInOrder :: IO()
testInOrder = 
    if inOrder testTree' == [1, 2, 3]
        then putStrLn "In-order cool!"
        else putStrLn "Something is wrong."

testPostOrder :: IO()
testPostOrder = 
    if postOrder testTree' == [1, 3, 2]
        then putStrLn "Post-order cool!"
        else putStrLn "Something is wrong."

main :: IO()
main = do
    testPreOrder
    testInOrder
    testPostOrder

-- Foldr for binary tree (any traversal order is fine)

foldTree :: (a -> b -> b) -> b -> BinaryTree a -> b
foldTree f acc Leaf = acc
foldTree f acc (Node left root right) = foldTree f z right where
    y = f root acc
    z = foldTree f y left

foldTree' :: (a -> b -> b) -> b -> BinaryTree a -> b
foldTree' f acc Leaf = acc
foldTree' f acc tree = foldr f acc (inOrder tree)