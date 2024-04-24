-- Las funciones curried son aquellas que toman sus argumentos de manera "parcial"
-- Función de suma curried
add :: Int -> Int -> Int
add x y = x + y

-- Aplicando la función de suma de manera parcial
add2 :: Int -> Int
add2 = add 2

-- Usando la función parcialmente aplicada
result :: Int
result = add2 3 -- Esto dará 5

-- Función de potencia curried
power :: Int -> Int -> Int
power x y = x ^ y

-- Aplicando la función de potencia de manera parcial
square :: Int -> Int
square = power 2

-- Usando la función parcialmente aplicada
result2 :: Int
result2 = square 3 -- Esto dará 9

-- Función de concatenación de cadenas curried
concatenate :: String -> String -> String
concatenate str1 str2 = str1 ++ str2

-- Aplicando la función de concatenación de manera parcial
greet :: String -> String
greet = concatenate "Hello, "

-- Usando la función parcialmente aplicada
message :: String
message = greet "John" -- Esto dará "Hello, John"

-- Función de resta curried
subtract :: Int -> Int -> Int
subtract x y = x - y

-- Aplicando la función de resta de manera parcial
subtract5 :: Int -> Int
subtract5 = Main.subtract 5

-- Usando la función parcialmente aplicada
result3 :: Int
result3 = subtract5 3 -- Esto dará 2

-- Función de división curried
divide :: Float -> Float -> Float
divide x y = x / y

-- Aplicando la función de división de manera parcial
divideBy2 :: Float -> Float
divideBy2 = divide 2.0

-- Usando la función parcialmente aplicada
result4 :: Float
result4 = divideBy2 10.0 -- Esto dará 5.0

-- Función para sumar los elementos de una lista
sumList :: [Int] -> Int
sumList = foldr (+) 0

-- Función para multiplicar los elementos de una lista
productList :: [Int] -> Int
productList = foldr (*) 1

-- Función para aplicar una operación a todos los elementos de una lista
mapList :: (Int -> Int) -> [Int] -> [Int]
mapList _ [] = []
mapList f (x:xs) = f x : mapList f xs

-- Función para filtrar los elementos de una lista según un predicado
filterList :: (Int -> Bool) -> [Int] -> [Int]
filterList _ [] = []
filterList p (x:xs)
  | p x       = x : filterList p xs
  | otherwise = filterList p xs

-- Función para aplicar una lista de funciones a un valor
applyFunctions :: [Int -> Int] -> Int -> [Int]
applyFunctions fs x = map ($ x) fs

-- Ejemplo de uso de las funciones curried
exampleList :: [Int]
exampleList = [1, 2, 3, 4, 5]

-- Sumar los elementos de la lista
sumResult :: Int
sumResult = sumList exampleList -- Esto dará 15

-- Multiplicar los elementos de la lista
productResult :: Int
productResult = productList exampleList -- Esto dará 120

-- Duplicar todos los elementos de la lista
doubleList :: [Int]
doubleList = mapList (*2) exampleList -- Esto dará [2, 4, 6, 8, 10]

-- Filtrar los elementos pares de la lista
evenNumbers :: [Int]
evenNumbers = filterList even exampleList -- Esto dará [2, 4]

-- Aplicar una lista de funciones a un valor
applyResult :: [Int]
applyResult = applyFunctions [(+1), (*2), (^2)] 3 -- Esto dará [4, 6, 9]
