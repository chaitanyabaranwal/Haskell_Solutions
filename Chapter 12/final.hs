data BinaryTree a = 
    Leaf | Node (BinaryTree a) a (BinaryTree a) deriving (Eq, Ord, Show)

unfold :: (a -> Maybe (a, b, a)) -> a -> BinaryTree b
unfold f root = case (f root) of
    Nothing -> Leaf
    Just (x, y, z) -> Node (unfold f x) y (unfold f z)

treeBuild :: Integer -> BinaryTree Integer
treeBuild n = unfold (\a -> if a == n then Nothing else Just (a+1, a, a+1)) 0
