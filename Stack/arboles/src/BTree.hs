module BTree where

data Tree a = Empty | Node (Tree a) a (Tree a) deriving (Show, Eq)

btree :: Tree Integer
btree = Node (Node Empty 3 Empty) 5 (Node (Node Empty 6 Empty) 8 (Node Empty 9 Empty))


treeSize :: Tree a -> Integer
treeSize Empty = 0
treeSize (Node l _ r) = 1 + treeSize l + treeSize r


treeSum :: Tree Integer -> Integer
treeSum Empty = 0
treeSum (Node l v r) = v + treeSum l + treeSum r

treeDepth :: Tree a -> Integer
treeDepth Empty = 0
treeDepth (Node l _ r) = 1 + max (treeDepth l) (treeDepth r)

flatten :: Tree a -> [a]
flatten Empty = []
flatten (Node l v r) = flatten l ++ [v] ++ flatten r

-- treeFold :: (a -> b -> b) -> b -> Tree a -> b  ???

treeFold :: b -> (b -> a -> b -> b) -> Tree a -> b
treeFold i _ Empty = i
treeFold i f (Node l v r) = f (treeFold i f l) v (treeFold i f r)

treeSize' :: Tree a -> Integer
treeSize' = treeFold 0 (\l _ r -> 1 + l + r)

treeSum' :: Tree Integer -> Integer
treeSum' = treeFold 0 (\l v r -> v + l + r)

treeDepth' :: Tree a -> Integer
treeDepth' = treeFold 0 (\l _ r -> 1 + max l r)

flatten' :: Tree a -> [a]
flatten' = treeFold [] (\l v r -> l ++ [v] ++ r)


