module Introducion where

import Data.Char(ord, chr)
import GHC.Base (TrName(TrNameD))

suma :: Int -> Int -> Int
suma a b = a + b

suma' :: Float -> Float -> Float
suma' a b = a + b

unChar :: Char
unChar = 'A'

unChar3 :: Char
unChar3 = '3'

tuplaDeCoordenadas :: (Integer, Integer)
tuplaDeCoordenadas = (3, 2)

                -- Tupla
tupla :: Char -> (Char, Int)
tupla x = ( x, ord x)


sumaTupla :: (Int, Int) -> Int
sumaTupla (a, b) = a + b

sumaTupla2 :: (Int, Int, Char) -> Int
sumaTupla2 (a, b, c) = a + b

sumaLista :: [Int] -> Int
sumaLista xs = sum (xs)


sumaLista' :: [Int] -> Int
sumaLista' = sum

lista :: [Int]
lista = [1..30]

esPar ::  Int  -> Bool
esPar n = even n

filtrarPares :: [Int] -> [Int]
filtrarPares xs = filter esPar  xs

mySplit :: Int -> [Int] -> ([Int], [Int])
mySplit n xs = splitAt n xs

splitTrainer :: Int -> [Int] -> ([Int], [Int])
splitTrainer n xs = (take n xs, drop n xs)

myAbs :: Int -> Int
myAbs n = if n >= 0 then n else -n


validPositiveNumber :: Int -> Int
validPositiveNumber n
  | n < 0 = -1
  | n == 0 = 0
  | otherwise = 1

myAbs' :: Int -> Int
myAbs' n | n >= 0 = n
         | otherwise = -n

validPositiveNumber' :: Int -> Int
validPositiveNumber' n | n < 0 = -1
                        |  n == 0 = 0
                        | n == 0 = 10
                        | otherwise = 1

-- Pattern matching

negar :: Bool -> Bool 
negar True = False
negar False = True

esUno :: Int -> Bool
esUno 1 = True
esUno _ = False

-- Si es uno el output es True, pero si es cualqui cosa es False

esUnoODos :: Int -> String
esUnoODos 1 = "one"
esUnoODos 2 = "dos"
esUnoODos _ = "No se que es :("

-- funcion anonima - Lambda Function
add :: Int -> (Int -> Int)
add = \x -> (\y -> x + y)

-- Vamos a hacer una lista de funciones donde pasemos expresiones + - * /

myFunction :: [(Int -> Int -> Int)]
myFunction = [(+), (-), (*), div, suma]

getFunction :: Char -> (Int -> Int -> Int)
getFunction e | e == '+' = myFunction !! 0
              | e == '-' = myFunction !! 1
              | e == '*' = myFunction !! 2
              | e == '/' = myFunction !! 3
              | otherwise = myFunction !! 4

-- myExp :: Char -> ( Int -> Int -> Int)
-- myExp = getFunction 

-- Podemos hacer funciones dentro de otras funciones, asi funciona como un private dentro de esa funcion, osea solo se llama dentro de esa funcio, no son accesibles.

-- Curried Function. Dar ejemplos para el lunes
-- (((multThree 1) 2 ) 3) 

multThree :: Int -> ( Int -> (Int ->Int) )
multThree x y z = x * y * z

-- { x ** 2 | x E {1..5}}
-- {x ** 2 | x <- [1..5]}
-- Combinatoria de todo. Es como hacer una doble iteracion de las dos listas.
-- [(x,y) | x <- [1..5], y <- [1..5]]
-- [(1,1),(1,2),(1,3),(1,4),(1,5),(2,1),(2,2),(2,3),(2,4),(2,5),(3,1),(3,2),(3,3),(3,4),(3,5),(4,1),(4,2),(4,3),(4,4),(4,5),(5,1),(5,2),(5,3),(5,4),(5,5)]

quicksort :: Ord a => [a] -> [a]
quicksort [] = []
quicksort (x:xs) =
    let smallerSorted = quicksort [a | a <- xs, a <= x]
        biggerSorted = quicksort [a | a <- xs, a > x]
    in  smallerSorted ++ [x] ++ biggerSorted


-- Bater matching ? si esta vacio devuelves vacio
quicksort1 :: [Int] -> [Int]
quicksort1 [] = []
quicksort1 (x:xs) = (quicksort1 menor) ++ [x] ++ (quicksort1 mayor )
                  where 
                    menor = [me | me <- xs, me < x]
                    mayor = [ma | ma <- xs, ma >= x]

-- all :: (a -> Bool) -> [a] -> Bool
-- raiz cuadrada sqrt 
-- fromIntegral :: (Integral a, Num b) => a -> b -> nos da un tipo numerico
-- floor :: (RealFrac a, Integral b) => a -> b -> redondeo hacia abajo
esPrimo :: Int -> Bool
esPrimo x | x > 1 && div x 1 == x && div x x == 1 && todosNoDivisibles x = True
          | otherwise = False
    where
      todosNoDivisibles x = all (\y -> x `mod` y /= 0) [2..limite]
      limite = floor (sqrt (fromIntegral x))

--getPrim :: Int -> [Int]

--getPrim n = filter esPrimo [2..n]

-- Ejemplo: getPrimos 20
-- 2,3,5,7,11,13,17,19
-- esPrimo :: Int -> Bool
-- esPrimo x = x > 1 && all (\y -> x `mod` y /= 0) [2..(x-1)]


esPrimoOP :: Int -> Bool
esPrimoOP 1 = False
esPrimoOP n = esPrimoAux n 2

esPrimoAux :: Int -> Int -> Bool
esPrimoAux n divisor 
    | divisor * divisor > n = True
    | mod n divisor == 0 = False
    | otherwise = esPrimoAux n (divisor + 1)
