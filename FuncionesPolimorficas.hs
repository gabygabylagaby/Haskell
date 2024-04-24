mySum :: Num a => [a] -> a
mySum = foldr (*) 1

-- :t zipWith
-- :t takewhile
--{ ghci> foldl (+) 0 [1..10] (Nota: Recibe 2 argumentos el ((0 + 1) + 2) + 3 es asi como funciona ) se evalua el 0 primero por que es el caso base, el caso base esta en la izquierda
-- 55
-- ghci> foldr (+) 0 [1..10] (Nota: Recibe 2 argumentos el 1 + (2 + (3 + 0)) es asi como funciona ) aqui inicia del 1 por que el caso base esta a la derecha 
-- 55
--}

square :: Int -> Int
square x = x * x

twiceSquare :: Int -> Int
-- twiceSquare x = square (square x)
twiceSquare = square . square
-- (.) -> se llama funcion de composicion
-- (show . square) 3
twiceSquare' :: Int -> String
twiceSquare' = show . square

