data Tree a = Null | Node a (Tree a) (Tree a)
    deriving(Read, Show)

addNode :: Ord a => a -> Tree a -> Tree a
addNode a Null = Node a Null Null

addNode a (Node n t1 t2)
    | a < n = Node n (addNode a t1) t2
    | otherwise = Node n t1 (addNode a t2) 


makeTree :: Ord a => [a] -> Tree a

makeTree [] = Null

makeTree (x : xs)
    = addNode x (makeTree xs)

inOrder :: Tree a -> [a]

inOrder Null = []

inOrder (Node a t1 t2)
    = (inOrder t1) ++ [a] ++ (inOrder t2)

mpSort :: Ord a => [a] -> [a]

mpSort = []

mpSort (x : xs)
    = inOrder (makeTree (x : xs))