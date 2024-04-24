module Clase_03_13_2024.DataTypes where
import Control.Concurrent (yield)
import Control.Arrow (ArrowChoice(left, right))

-- Arbol binario
-- Tipos de datos recursivos
data Nat = Zero | Succ Nat deriving (Show, Eq)

-- Guarda un tipo de dato que se pide así mismo, se puede autollamar
-- ghci> Zero
-- ghci> Succ Zero
-- ghci> Succ (Succ Zero)

natToNum :: Nat -> Int
natToNum Zero = 0
natToNum (Succ n) = 1 + natToNum n

-- ghci> natToNum $ Succ (Succ (Succ Zero))
-- 3

numToNat :: Int -> Nat
numToNat 0 = Zero
numToNat n = Succ $ numToNat (n - 1)

-- ghci> numToNat 10
-- Succ (Succ (Succ (Succ (Succ (Succ (Succ (Succ (Succ (Succ Zero)))))))))
-- ghci> natToNum $ numToNat 10
-- 10

suma :: Nat -> Nat -> Nat
suma n m = numToNat (natToNum n + natToNum m)

-- ghci> suma (numToNat 2) (numToNat 3)
-- Succ (Succ (Succ (Succ (Succ Zero))))

-- ghci> natToNum $ suma (numToNat 2) (numToNat 3)
-- 5

suma2 :: Nat -> Nat -> Nat
suma2 Zero n = n
suma2 (Succ m) n = Succ (suma2 m n)

{- data Nat = Zero 
         | Succ Nat
         deriving (Show) -}




-- ghci> natToNum $ suma2 (numToNat 2) (numToNat 3)
-- 5

-- Representación de árbol:
data Tree a
  = Empty
  | Leaf a
  | Node (Tree a) a (Tree a)
  deriving (Show)

data List a = Vacio 
             | Siguiente a (List a)
             deriving (Show)


myList :: List String
myList = Siguiente "manzana" (Siguiente "Limones" (Vacio))

len :: List a -> Int
len Vacio = 0
len (Siguiente _ xs) = 1 + len xs

myNum :: Nat
myNum = Succ (Succ Zero)

-- Representación del arbol:
{- 
                7
        5               9
    3       6
-}
arbol :: Tree Int
arbol = Node (Node (Leaf 3) 5 (Leaf 6)) 7 (Leaf 9)

addNode :: Ord a => Tree a -> a -> Tree a
addNode Empty x = Leaf x
addNode (Leaf x) y
    | y <= x = Node (Leaf y) x Empty
    | otherwise = Node Empty x (Leaf y)
addNode (Node left x right) y
    | y <= x = Node (addNode left y) x right
    | otherwise = Node left x (addNode right y)

inOrden :: Tree a -> [a]
inOrden Empty = []
inOrden (Leaf x) = [x]
inOrden (Node left x right) = inOrden left ++ [x] ++ inOrden right

preOrden :: Tree a -> [a]
preOrden Empty = []
preOrden (Leaf x) = [x]
preOrden (Node left x right) = [x]

postOrden :: Tree a -> [a]
postOrden Empty = []
postOrden (Leaf x) = [x]
postOrden (Node left x right) = [x]


instance Eq a => Eq (Tree a) where
  (Leaf x) == (Leaf y) = x == y
  (Node left1 val1 right1) == (Node left2 val2 right2) = 
    (left1 == left2) && (val1 == val2) && (right1 == right2)
  _ == _ = False


{- 
inOrden :: Tree a -> [a]
inOrden Empty = []
inOrden (Leaf x) = [x]
inOrden (Node left x right) = inOrden left ++ [x] ++ inOrden right

preOrden :: Tree a -> [a]
preOrden Empty = []
preOrden (Leaf x) = [x]
preOrden (Node left x right) = [x] ++ preOrden left ++ preOrden right

postOrden :: Tree a -> [a]
postOrden Empty = []
postOrden (Leaf x) = [x]
postOrden (Node left x right) = postOrden left ++ postOrden right ++ [x]
 -}


 