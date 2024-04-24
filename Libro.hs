import Data.ByteString (group)
import Data.Char
import Data.List


-- Exercises 3.11
-- 1. What are the types of the following values?
-- ['a','b','c'] :: [Char]
-- ('a','b','c') :: (Char,Char,Char)
-- [(False,'O'),(True,'1')] :: [(Bool,Char)]
-- ([False,True],['0','1']) :: ([Bool],[Char])
-- [tail, init, reverse] :: [[a] -> [a]]

{-
-- Write down definitions that have the following types; it does not matter what the definitions
actually do as long as they are type correct.
bools :: [Bool]
nums :: [[Int]]
add :: Int -> Int -> Int -> Int
copy :: a -> (a,a)
apply :: (a -> b) -> a -> b
-}

bools :: [Bool]
bools = [False, True]

nums :: [[Int]]
nums = [[1, 2], [3, 4], [5, 6]]

add :: Int -> Int -> Int -> Int
add x y z = x + y + z

copy :: a -> (a, a)
copy x = (x, x)

apply :: (t1 -> t2) -> t1 -> t2
apply f x = f x

{-
-- What are the types of the following functions?
second xs = head (tail xs)
swap (x,y) = (y,x)
pair x y = (x,y)
double x = x*2
palindrome xs = reverse xs == xs
twice f x = f (f x)
Hint: take care to include the necessary class constraints in the types if the functions are defined
using overloaded operators.
-}
-- 1. Function: second xs = head (tail xs)
second :: [a] -> a
second xs = head (tail xs)

-- 2. Function: swap (x,y) = (y,x)
swap :: (a, b) -> (b, a)
swap (x, y) = (y, x)

-- 3. Function: pair x y = (x,y)
pair :: a -> b -> (a, b)
pair x y = (x, y)

-- 4. Function: double x = x*2
double :: (Num a) => a -> a
double x = x * 2

-- 5. Function: palindrome xs = reverse xs == xs
palindrome :: (Eq a) => [a] -> Bool
palindrome xs = reverse xs == xs

-- 6. Function: twice f x = f (f x)
twice :: (a -> a) -> a -> a
twice f x = f (f x)

{-
-- Why is it not feasible in general for function types to be instances of the Eq class? When is it feasible? Hint: two functions of the same type are equal if they always return equal results for equal arguments.
En general, no es factible que los tipos de funciones sean instancias de la clase Eq porque determinar la igualdad entre funciones puede ser bastante complejo.

-}

{-
-- Without using any other library functions or operators, show how the meaning of the following
pattern matching definition for logical conjunction && can be formalised using conditional
expressions:
True && True = True
False &&  _=False
Hint: use two nested conditional expressions.
-}
yLogico :: Bool -> Bool -> Bool
yLogico x y =
  if x == True
    then
      if y == True
        then True
        else False
    else False

{-
-- Do the same for the following alternative definition, and note the difference in the number of
conditional expressions that are required:
True && b = b
False && _ = False
-}
yLogico' :: Bool -> Bool -> Bool
yLogico' x y =
  if x == True
    then y
    else False

{-
-- . Show how the meaning of the following curried function definition can be formalised in terms of
lambda expressions:
mult :: Int -> Int -> Int -> Int
mult x y z = x*y*z
-}
mult :: Int -> Int -> Int -> Int
-- es una lambda expresión que toma 'x' y devuelve otra lambda expresión que toma 'y' y devuelve otra lambda expresión que toma 'z' y finalmente realiza la multiplicación x * y * z.
mult = \x -> \y -> \z -> x * y * z

{-
-- The Luhn algorithm is used to check bank card numbers for simple errors such as mistyping a
digit, and proceeds as follows:
consider each digit as a separate number;
moving left, double every other number from the second last;
subtract 9 from each number that is now greater than 9;
add all the resulting numbers together;
if the total is divisible by 10, the card number is valid.
Define a function luhnDouble :: Int -> Int that doubles a digit and subtracts 9 if the result is
greater than 9. For example:
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
-- duplica un dígito y resta 9 si el resultado es mayor que 9:
luhnDouble :: Int -> Int
luhnDouble x = if doubled > 9 then doubled - 9 else doubled
  where
    doubled = x * 2

-- toma cuatro dígitos como argumentos y el total según la regla de Luhn.
-- Si el total es divisible por 10, la tarjeta es válida, la función devuelve True; de lo contrario, devuelve False.
luhn :: Int -> Int -> Int -> Int -> Bool
luhn a b c d = total `mod` 10 == 0
  where
    total = luhnDouble a + b + luhnDouble c + d

-- Chapter 5
-- 1. sum [(x^2) `div` 2 | x <- [1..100]]
-- 2
grid :: Int -> Int -> [(Int, Int)]
grid m n = [(x, y) | x <- [0 .. m], y <- [0 .. n]]

-- 3

square :: Int -> [(Int, Int)]
square n = [(x, y) | (x, y) <- grid n n, x /= y]

-- 4

replicate' :: Int -> a -> [a]
replicate' n x = [x | _ <- [1 .. n]]

-- 5

pyths :: Int -> [(Int, Int, Int)]
pyths limit =
  [ (x, y, z) | x <- [1 .. limit], y <- [1 .. limit], z <- [1 .. limit], x ^ 2 + y ^ 2 == z ^ 2
  ]

-- 6

factors :: Int -> [Int]
factors n = [x | x <- [1 .. n - 1], n `mod` x == 0]

perfects :: Int -> [Int]
perfects limit = [n | n <- [1 .. limit], sum (factors n) == n]

-- Example usage:
-- perfects 500

-- 7
-- concat [(concat [[(x,y) | y <- [3,4]] | x <- [1,2]])]

-- 8

positions :: (Eq a) => a -> [a] -> [Int]
positions x xs = findIndices 0 xs
  where
    findIndices _ [] = []
    findIndices idx (y : ys)
      | x == y = idx : rest
      | otherwise = rest
      where
        rest = map (+ 1) (findIndices (idx + 1) ys)

-- 9
scalarproduct :: [Int] -> [Int] -> Int
scalarproduct xs ys = sum [x * y | (x, y) <- zip xs ys]

-- chapter 6
-- 1
{--

length :: [a] -> Int
length [] = 0
length (_:xs) = 1 + length xs

-- 2

-- a. Decide if all logical values in a list are True
and' :: [Bool] -> Bool
and' [] = True
and' (x:xs) = x && and' xs

-- b. Concatenate a list of lists
concat' :: [[a]] -> [a]
concat' [] = []
concat' (x:xs) = x ++ concat' xs

-- c. Produce a list with n identical elements
replicate' :: Int -> a -> [a]
replicate' 0 _ = []
replicate' n x = x : replicate' (n-1) x

-- d. Select the nth element of a list
(!!) :: [a] -> Int -> a
(x:xs) !! n
  | n == 0 = x
  | otherwise = xs !! (n-1)

-- e. Decide if a value is an element of a list
elem' :: Eq a => a -> [a] -> Bool
elem' _ [] = False
elem' y (x:xs)
  | y == x    = True
  | otherwise = elem' y xs

-- f. Merge two sorted lists into one sorted list
merge :: Ord a => [a] -> [a] -> [a]
merge [] ys = ys
merge xs [] = xs
merge (x:xs) (y:ys)
  | x <= y    = x : merge xs (y:ys)
  | otherwise = y : merge (x:xs) ys

-- a. Decide if all logical values in a list are True
and' :: [Bool] -> Bool
and' [] = True
and' (x:xs) = x && and' xs

-- b. Concatenate a list of lists
concat' :: [[a]] -> [a]
concat' [] = []
concat' (x:xs) = x ++ concat' xs

-- c. Produce a list with n identical elements
replicate' :: Int -> a -> [a]
replicate' 0 _ = []
replicate' n x = x : replicate' (n-1) x

-- d. Select the nth element of a list
(!!) :: [a] -> Int -> a
(x:xs) !! n
  | n == 0 = x
  | otherwise = xs !! (n-1)

-- e. Decide if a value is an element of a list
elem' :: Eq a => a -> [a] -> Bool
elem' _ [] = False
elem' y (x:xs)
  | y == x    = True
  | otherwise = elem' y xs

-- f. Merge two sorted lists into one sorted list
merge :: Ord a => [a] -> [a] -> [a]
merge [] ys = ys
merge xs [] = xs
merge (x:xs) (y:ys)
  | x <= y    = x : merge xs (y:ys)
  | otherwise = y : merge (x:xs) ys

-- 4

merge :: Ord a => [a] -> [a] -> [a]
merge [] ys = ys
merge xs [] = xs
merge (x:xs) (y:ys)
    | x <= y    = x : merge xs (y:ys)
    | otherwise = y : merge (x:xs) ys

--5

merge :: Ord a => [a] -> [a] -> [a]
merge [] ys = ys
merge xs [] = xs
merge (x:xs) (y:ys)
    | x <= y    = x : merge xs (y:ys)
    | otherwise = y : merge (x:xs) ys

-- 6

-- a. Calculate the sum of a list of numbers:
sumList :: Num a => [a] -> a
sumList [] = 0
sumList (x:xs) = x + sumList xs

-- b. Take a given number of elements from the start of a list:
takeElements :: Int -> [a] -> [a]
takeElements 0 _ = []
takeElements _ [] = []
takeElements n (x:xs) = x : takeElements (n-1) xs

-- c. Select the last element of a non-empty list:
lastElement :: [a] -> a
lastElement [x] = x
lastElement (_:xs) = lastElement xs

-}

-- 1
type Bit = Int

unfold :: (t -> Bool) -> (t -> a) -> (t -> t) -> t -> [a]
unfold p h t x
  | p x = []
  | otherwise = h x : unfold p h t (t x)

int2bin :: Int -> [Bit]
int2bin = unfold (== 0) (`mod` 2) (`div` 2)

chop8 :: [Bit] -> [[Bit]]
chop8 = unfold null (take 8) (drop 8)

map' :: (a -> b) -> [a] -> [b]
map' f = unfold null (f . head) tail

iterate' :: (a -> a) -> a -> [a]
iterate' = unfold (const False) id

-- 2

type Bit = Int

bin2int :: [Bit] -> Int
bin2int bits = sum [w * b | (w, b) <- zip weights bits]
  where
    weights = iterate (* 2) 1

bin2int' :: [Bit] -> Int
bin2int' = foldr (\x y -> x + 2 * y) 0

int2bin :: Int -> [Bit]
int2bin 0 = []
int2bin n = n `mod` 2 : int2bin (n `div` 2)

make8 :: [Bit] -> [Bit]
make8 bits = take 8 (bits ++ repeat 0)

parity :: [Bit] -> Bit
parity bits = sum bits `mod` 2

addParity :: [Bit] -> [Bit]
addParity bits = parity bits : bits

encode :: String -> [Bit]
encode = concatMap (addParity . make8 . int2bin . ord)

chop9 :: [Bit] -> [[Bit]]
chop9 [] = []
chop9 bits = take 9 bits : chop9 (drop 9 bits)

checkParity :: [Bit] -> [Bit]
checkParity bits
  | head bits == parity (tail bits) = tail bits
  | otherwise = error "Parity Error"

decode :: [Bit] -> String
decode = map (chr . bin2int . checkParity) . chop9

channel :: [Bit] -> [Bit]
channel = id

transmit :: String -> String
transmit = decode . channel . encode

-- 4

altMap :: (a -> b) -> (a -> b) -> [a] -> [b]
altMap _ _ [] = []
altMap f0 f1 (x:xs) = f0 x : altMap f1 f0 xs

-- 5


votes :: [String]
votes = ["Red", "Blue", "Green", "Blue", "Blue", "Red"]

count :: Eq a => a -> [a] -> Int
count x = length . filter (== x)

rmdups :: Eq a => [a] -> [a]
rmdups [] = []
rmdups (x:xs) = x : rmdups (filter (/= x) xs)

result :: Ord a => [a] -> [(Int, a)]
result vs = sort [(count v vs, v) | v <- rmdups vs]

winner :: Ord a => [a] -> a
winner = snd . last . result

ballots :: [[String]]
ballots =
  [ ["Red", "Green"]
  , ["Blue"]
  , ["Green", "Red", "Blue"]
  , ["Blue", "Green", "Red"]
  , ["Green"]
  ]

rmemtpy :: Eq a => [[a]] -> [[a]]
rmemtpy = filter (/= [])

elim :: Eq a => a -> [[a]] -> [[a]]
elim x = map (filter (/= x))

rank :: Ord a => [[a]] -> [a]
rank = map snd . result . map head

winner' :: Ord a => [[a]] -> a
winner' bs =
  case rank (rmemtpy bs) of
    [c] -> c
    (c:cs) -> winner' (elim c bs)

-- Ejercicio

square' :: Int -> Int
square' x = x * x

twiceSquare :: Int -> Int
-- twiceSquare x = square (square x)
twiceSquare = square' . square'

twiceSquare' :: Int -> String
twiceSquare' = show . square

-- Función para ordenar una lista
sort' :: (Ord a) => [a] -> [a]
sort' [] = []
sort' (x : xs) = insert' x (sort' xs)
  where
    insert' x [] = [x]
    insert' x (y : ys)
      | x <= y = x : y : ys
      | otherwise = y : insert' x ys

-- Función para agrupar elementos adyacentes iguales en una lista
group' :: (Eq a) => [a] -> [[a]]
group' [] = []
group' (x : xs) = (x : takeWhile (== x) xs) : group' (dropWhile (== x) xs)

-- Función para contar la frecuencia de cada elemento en la lista
frequency :: [Int] -> [(Int, Int)]
frequency xs = map (\x -> (head x, length x)) $ group' $ sort' xs

-- Función para convertir una frecuencia en una fila del histograma
row :: Int -> Int -> String
row n freq = show n ++ ": " ++ replicate freq '*'

-- Función principal para crear el histograma
histograma :: [Int] -> [String]
histograma xs = map (\(n, freq) -> row n freq) $ frequency xs
