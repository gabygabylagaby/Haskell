{-
-- El algoritmo de Luhn se utiliza para comprobar los números de tarjetas bancarias en busca de errores simples, 
como escribir mal un dígito y procede de la siguiente manera:
considere cada dígito como un número separado;
moviéndose hacia la izquierda, duplique cada dos números desde el penúltimo;
restar 9 de cada número que ahora sea mayor que 9;
sume todos los números resultantes;
si el total es divisible por 10, el número de tarjeta es válido.
Defina una función luhnDouble :: Int -> Int que duplique un dígito y reste 9 si el resultado es
mayor que 9. Por ejemplo:
> luhnDouble 3
6
> luhnDouble 6
3
Using luhnDouble and the integer remainder function mod, define a function luhn :: Int ->
Int -> Int -> Int -> Bool that decides if a four-digit bank card number is valid. For
example:
> luhn 1 7 8 4
True
> luhn 4 7 8 3
False
-}
--duplica un dígito y resta 9 si el resultado es mayor que 9:
luhnDouble :: Int -> Int
luhnDouble x = if doubled > 9 then doubled - 9 else doubled
    where doubled = x * 2

-- toma cuatro dígitos como argumentos y el total según la regla de Luhn. 
-- Si el total es divisible por 10, la tarjeta es válida, la función devuelve True; de lo contrario, devuelve False.
luhn :: Int -> Int -> Int -> Int -> Bool
luhn a b c d = total `mod` 10 == 0
    where total = luhnDouble a + b + luhnDouble c + d

{--
¿Alguna vez se preguntó cómo los sitios web validan los números de tarjetas de crédito durante las compras en línea? En lugar de consultar una base de datos extensa o confiar en la magia, la mayoría
Los proveedores de crédito emplean una fórmula de suma de verificación para diferenciar los números válidos de los
colecciones de dígitos aleatorios o errores de entrada.
Algoritmo de validación de tarjeta de crédito:
1. Duplique el valor de cada segundo dígito comenzando por la derecha.
Es decir, el último dígito no cambia; el penúltimo dígito se duplica; el
el antepenúltimo dígito no cambia; etcétera. Por ejemplo, [1,3,8,6] se convierte
[2,3,16,6].
2. Sume los dígitos de los valores duplicados y los dígitos no duplicados del
número original. Por ejemplo, [2,3,16,6] se convierte en 2 + 3 + 1 + 6 + 6 = 18.
3. Calcula el resto cuando la suma se divide por 10. Para lo anterior
Por ejemplo, el resto sería 8. Si el resultado es igual a 0, entonces el número
es válida
-}
-- toma el cociente entero de n dividido por 10, lo que básicamente elimina el último dígito de n. Luego, 
-- ++ [n mod 10] agrega el último dígito de n a la lista resultante.

toDigits :: Integer -> [Integer]
toDigits 0 = []
toDigits n
  | n < 0 = []
  | otherwise = toDigits (n `div` 10) ++ [n `mod` 10]

-- 
toDigitsRev :: Integer -> [Integer]
toDigitsRev = reverse . toDigits


doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther doubleDigits = reverse (doubleEveryOther' ( reverse doubleDigits))
            where 
                doubleEveryOther' [] = []
                doubleEveryOther' [x] = [x]
                doubleEveryOther' (x:y:zs) = x : 2* y : doubleEveryOther' zs

sumDigits :: [Integer] -> Integer
sumDigits [] = 0
sumDigits (x:xs) = sum (toDigits x) + sumDigits xs


validate :: Integer -> Bool
validate n = (sumDigits $ doubleEveryOther $ toDigits n) `mod` 10 == 0
