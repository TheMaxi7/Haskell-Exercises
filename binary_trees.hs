data BST a = Void | Node {
    val :: a,
    left, right :: BST a
} deriving (Eq, Ord, Read, Show)

exampleTree1 :: BST Int
exampleTree1 =
    Node 1
        (Node 2
            (Node 4 Void Void)
            (Node 5 Void Void))
        (Node 3 Void Void)

exampleTree2 :: BST Int
exampleTree2 =
    Node 5
        (Node 4
            (Node 3 Void Void)
            (Node 5 Void Void))
        (Node 6 Void Void)

exampleVoidTree :: BST Int
exampleVoidTree = Void




sumBST :: Num a => BST a -> a
sumBST Void = 0
sumBST (Node val left right) = val + sumBST left + sumBST right

sumOddBST :: Integral a => BST a -> a
sumOddBST Void = 0
sumOddBST (Node val left right)
    |odd val = val + sumOddBST left + sumOddBST right
    |otherwise = sumOddBST left + sumOddBST right

sameSums :: (Eq a, Num a) => [BST a] -> Bool
sameSums [] = True
sameSums [_] = True
sameSums (x:y:xs)
    | sumBST x == sumBST y = sameSums (y:xs)
    | otherwise = False

bstElem :: (Eq a, Num a, Ord a) => a -> BST a -> Bool
bstElem x Void = False
bstElem x (Node val left right)
    |x < val = bstElem x left
    |x > val = bstElem x right
    |otherwise = True

insertElem :: (Eq a, Num a, Ord a) => a -> BST a -> BST a 
insertElem x Void = Node x Void Void
insertElem x (Node val left right)
    | x == val  = Node val left right 
    | x < val   = Node val (insertElem x left) right
    | otherwise = Node val left (insertElem x right)


bst2list :: (Eq a, Num a, Ord a) => BST a-> [a]
bst2list Void = []
bst2list (Node val left right) = [val] ++ bst2list left ++ bst2list right


filterTree :: (Eq a, Num a, Ord a) => (a -> Bool) -> BST a -> [a]
filterTree p Void = []
filterTree p (Node val left right)
    | p val = [val] ++ filterTree p left ++ filterTree p right
    | otherwise = []++ filterTree p left ++ filterTree p right