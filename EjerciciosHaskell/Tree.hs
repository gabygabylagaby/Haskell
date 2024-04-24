module Main where

-- Este arbol es un arbol binario de enteros, donde cada nodo tiene un valor y dos hijos, left y right.
data Tree a
    = Empty
    | Node a (Tree a) (Tree a)
    deriving (Show)

-- Función para construir un árbol a partir de una lista de listas de enteros 
build :: [[Int]] -> Tree Int
build [] = Empty
build xs = node xs 1
  where
    node xs i
      | i > length xs || i == -1 = Empty -- Si el índice es mayor que la longitud de la lista o es -1, devolvemos un nodo vacío
      | otherwise = Node i (node xs leftChild) (node xs rightChild) -- Si no, creamos un nodo con el valor i y sus hijos izquierdo y derecho
      where 
        leftChild = if leftChildIndex > 0 then leftChildIndex else -1
        rightChild = if rightChildIndex > 0 then rightChildIndex else -1
        leftChildIndex = xs !! (i - 1) !! 0
        rightChildIndex = xs !! (i - 1) !! 1
                                              

-- Función para intercambiar los hijos de un nodo en una profundidad dada
swap :: Tree a -> Int -> Tree a
swap Empty _ = Empty
swap (Node value left right) depth
  | depth <= 1 = swapNode  -- Si la profundidad es 1 o menor, intercambiamos los hijos
  | otherwise = Node value (swapLeft) (swapRight) -- Si no, seguimos bajando en el árbol
  where
    swapNode = Node value right left 
    swapLeft = swap left (depth - 1)
    swapRight = swap right (depth - 1)

swapAmount :: [Int] -> Tree Int -> Tree Int
swapAmount [] tree = tree
swapAmount [x] tree = tree
swapAmount (x:y:xs) tree = swapAmount xs (swap (swap tree x) y)

inorder :: Tree a -> [a]
inorder Empty = []
inorder (Node val left right) = inorder left ++ [val] ++ inorder right

-- Arboles creados:

-- Árboles creados
tree11 :: Tree String
tree11 = Node "1" (Node "2" Empty Empty) (Node "3" Empty Empty)

tree22 :: Tree String
tree22 = Node "1" (Node "2" Empty (Node "4" Empty Empty)) (Node "3" (Node "5" Empty Empty) Empty)

tree33 :: Tree String
tree33 = Node "1"
          (Node "2"
            (Node "4" Empty Empty)
            (Node "3"
              (Node "6" Empty Empty)
              (Node "7"
                (Node "8" Empty Empty)
                (Node "9"
                  (Node "10" Empty Empty)
                  (Node "11" Empty Empty)))))
          Empty


{-- Esta bien formado
    1         1
   / \      / \
  2   3    3  2
  
  Inorden Original: 2 1 3

-}
tree1 :: Tree Int
tree1 = build [[2, 3], [(-1), (-1)], [(-1), (-1)]]


{-- esta bien formado
    1                         1
   / \                       / \   
  2     3                 2     3
 / \     / \            / \     / \ 
-1   4  -1  5        4    -1   5   -1
    / \    / \           / \       / \    
  -1  -1  -1 -1        -1 -1     -1 -1

  Inorden Original: 2 4 1 3 5
 --}

tree2 :: Tree Int
tree2 = build [[2, 3], [(-1), 4], [(-1), 5], [(-1), (-1)], [(-1), (-1)]]

{-- esta bien formado

       1                         
    /    \                    
    2     3                 
 /  \     / \           
 4   -1   5   -1         
 / \      /  \           
 6 -1     7    8        
/ \      / \  / \    
-1  9  -1 -1  10 11    

  Inorden Original: 6 9 4 2 1 7 5 10 8 11 3

-}
tree3 :: Tree Int
tree3 = build [[2, 3], [4, (-1)], [5, (-1)], [6, (-1)], [7, 8], [(-1), 9], [(-1), (-1)], [(10), (11)], [(-1), (-1)], [(-1), (-1)], [(-1), (-1)]]

tree4 :: Tree Int
tree4 = build [[2, 3], [(-1), (-1)], [5, (-1)], [6, (-1)], [7, 8], [(-1), 9], [(-1), (-1)], [10, 11],  [12, 13], [14, 15], [(-1), (-1)], [(-1), (-1)], [(-1), (-1)]]

swapTree1 :: Tree Int -> Tree Int
swapTree1 = swapAmount [1, 1]

swapTree2 :: Tree Int -> Tree Int
swapTree2 = swapAmount [2]

swapTree3 :: Tree Int -> Tree Int
swapTree3 = swapAmount [2,4]


-- Función para imprimir la salida del árbol en orden
printInorder :: Tree Int -> IO ()
printInorder tree = putStrLn $ unwords $ map show $ inorder tree

main :: IO ()
main = do
  putStrLn "Test 1:"
  print (inorder $ swapTree1 tree1)
  putStrLn "Original:"
  printInorder tree1
  putStrLn "---------------------"

  putStrLn "Test 2:"
  print (inorder $ swapTree2 tree2) 
  putStrLn "Original:"
  printInorder tree2
  putStrLn "---------------------"

  putStrLn "Test 3:"
  print (inorder $ swapTree3 tree3)
  putStrLn "Original:"
  printInorder tree3
  putStrLn "---------------------"
