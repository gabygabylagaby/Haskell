module Example where 

suma3 :: Num a => a -> a -> a
suma3 a b = a + b

-- Recursion
factorial :: Integer -> Integer
factorial 0 = 1
factorial n = n * factorial (n - 1)

-- Recursion por listas 
myLength :: [a] -> Int
myLength [] = 0
myLength (_:xs) = 1 + myLength xs

-- xs todos excepto el primero de la lista
-- 3 [1,2,3,4,5,6]
-- [1,2,3,3,4,5,6]
insertVal :: Ord a => a -> [a] -> [a]
insertVal x [] =[x]
--        3  1 [2..6]
insertVal x (y:ys)
    | x <= y    = x : y : ys
    | otherwise = y : insertVal x ys

-- ys es el resto de la lista
-- (y:ys) -> esto lo esta iterando
-- x : y : ys = (x,y,ys) primer valor, segundo valor y el resto, esta es la forma de armar la lista
-- insertVal x ys -> osea que inserta el primer valor y el resto de los valores, esta volviendo a armar la lista 



-- Definición de la función myZip que toma dos listas y devuelve una lista de tuplas
myZip :: [a] -> [b] -> [(a,b)]

-- Caso base: Si la primera lista es vacía, el resultado es una lista vacía, no hay elementos para emparejar.
myZip [] _ = []

-- Caso base: Si la segunda lista es vacía, el resultado es una lista vacía, no hay elementos para emparejar.
myZip _ [] = []

-- Patrón para emparejar los primeros elementos de ambas listas y construir una tupla con ellos,
-- luego llamamos recursivamente a myZip con las listas restantes.
myZip (x:xs) (y:ys) = (x,y) : myZip xs ys

-- Funcion Poimorfica
-- Una funcion que se aplica dentro de una lista
-- :t map -> toma una funcion de una lista y te devuelve el mismo tamaño de la lista, Recibe una funcion y va a transformar esa lista al tipo de dato que estas retornando aesa funcion
-- ghci> map (*2) [1..10]
-- [2,4,6,8,10,12,14,16,18,20]
-- ghci> :t (*)
-- (*) :: Num a => a -> a -> a
-- ghci> :t map
-- map :: (a -> b) -> [a] -> [b]
--
-- Un ejemplo polimorfico es:
-- map show [1..10]
-- :t filter -> Buscara atravez de un filtro
-- :t foldr -> Es otra clase de tipo, Ord, Equ, Foldr, es un tipo de clase
-- Espera 2 argumentos y por eso vas a tener un b 
-- sumarLista :: Num a => [a] -> a
-- sumarLista xs = foldr (+) 0 xs
-- sumarLista [1,2,3,4,5]
-- 15

-- :t foldl