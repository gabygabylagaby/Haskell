module Monad where

-- Ejemplo de monad de Int a [Int]
doubleAndList :: Int -> [Int]
doubleAndList x = [x, x * 2] -- tiene que devolver 2 elemmentos, el que le das y la multiplicación por 2


doubleAll :: [Int] -> [Int]
doubleAll xs = xs >>= doubleAndList -- >>= es el bind, estructira de datos que maneja efectos secundarios. 
-- Concatena la lista de resultados de la función doubleAndList


