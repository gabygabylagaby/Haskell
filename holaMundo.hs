module Ejemplo where 
    
tuFechaDeNacimiento = 1987

tuNombre =  "Gaby"

doble x = 2 * x

dobleDeXMasUno x = doble x + 1

nextNum n = n + 1

square n = n * n

suma:: Int -> Int -> String

suma a b = show(a + b)

dividir :: Float -> Float -> Float
dividir x y = x / y

factorial :: Integer -> Integer
factorial 0 = 1
factorial n = n * factorial (n - 1)

cadena = "Hola chetos"

a = 'a'

{-
-- suma:: Int -> Int -> Int 
suma a b c =  a +b +c
-}


esPar ::  Int  -> Bool
esPar n = mod n 2 == 0

sumaGrande :: Integer -> Integer -> Integer

sumaGrande a b = a + b
