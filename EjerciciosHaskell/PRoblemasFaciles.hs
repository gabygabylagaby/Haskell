import Data.List (nub)
import Data.Functor

-- update list

updateAbsolute :: [Int] -> [Int]
updateAbsolute = map abs

-- Pascal's triangle

pascalTriangle :: Int -> [[Int]]
pascalTriangle n = take n $ iterate nextRow [1]
    where
        nextRow row = zipWith (+) (row ++ [0]) (0 : row)

printPascal :: [Int] -> [Int]
printPascal xs = [1] ++ row xs ++ [1]


row :: [Int] -> [Int]
row [] = []
row [_] = []
row (x:y:xs) = (x+y) : row (y:xs)

printPascalTriangle :: Int -> IO ()
printPascalTriangle n = mapM_ (putStrLn . unwords . map show . printPascal) (take n (iterate row [1]))

{- main :: IO ()
main = do
    n <- readLn :: IO Int
    printPascalTriangle n -}

-- Mingle String
mingledString :: String -> String -> String
mingledString [] [] = []
mingledString (x:xs) (y:ys) = x : y : mingledString xs ys

data Mingle a = Mingle [a] deriving Show

instance Functor Mingle where
    fmap f (Mingle []) = Mingle []
    fmap f (Mingle (x:xs)) = Mingle (f x : fmap f xs)

mingledString2 :: String -> String -> String
mingledString2 [] [] = []
mingledString2 (x:xs) (y:ys) = x : y : mingledString xs ys

main :: IO ()
main = do
    let mingled = mingledString2 "abcde" "pqrst"
    print mingled
    let mingled' = mingledString2 "hacker" "ranker"
    print mingled'
    let mingled'' = fmap (\c -> [c]) (Mingle mingled)
    print mingled''


-- string o permute
stringPermute :: String -> String
stringPermute [] = []
stringPermute [x] = [x]
stringPermute (x:y:xs) = y : x : stringPermute xs

-- convex Hull

-- String comprension 
-- Gabriela Garcia Villalobos

compressMessage :: String -> String
compressMessage [] = []
compressMessage [x] = [x]
compressMessage (x:xs) = compressed x 1 xs --mando caracter, contador y el resto de la cadena
  where
    compressed caracter count [] = if count == 1 then [caracter] else caracter : show count --si el contador es 1, solo mando el caracter, si no mando el caracter y el contador
    compressed caracter count (y:ys)
      | caracter == y    = compressed caracter (count + 1) ys -- si el caracter es igual al siguiente, aumento el contador
      | count == 1 = caracter : compressed y 1 ys -- si el contador es 1, mando el caracter y sigo con la cadena
      | otherwise = show count ++ compressed y 1 ys -- si el contador es mayor a 1, mando el contador y sigo con la cadena

-- super digit
-- Función para calcular el super dígito de un número
superDigit :: Integer -> Integer
superDigit n
    | n < 10    = n
    | otherwise = superDigit $ sumDigits n

-- Función para sumar los dígitos de un número
sumDigits :: Integer -> Integer
sumDigits 0 = 0
sumDigits n = n `mod` 10 + sumDigits (n `div` 10)

-- Función para calcular el super dígito de un número repetido k veces
repeatedSuperDigit :: Integer -> Integer -> Integer
repeatedSuperDigit n k = superDigit (n * k)

-- Rotate String
rotateLeft :: String -> String
rotateLeft [] = []
rotateLeft (x:xs) = xs ++ [x]

-- Función para generar todas las rotaciones de una cadena
rotations :: String -> [String]
rotations str = take (length str) (iterate rotateLeft str)

-- Remove Duplicates

isIn :: Char -> String -> Bool
isIn _ [] = False
isIn c (x:xs)
    | c == x    = True
    | otherwise = isIn c xs

removeDuplicates :: String -> String
removeDuplicates [] = []
removeDuplicates (x:xs)
    | x `isIn` xs = removeDuplicates xs  -- Si x está presente en xs, lo ignoramos
    | otherwise   = x : removeDuplicates xs  -- Si x no está presente en xs, lo mantenemos
 

removeDuplicates2 :: String -> String
removeDuplicates2 = nub

data Tree a
  = Empty
  | Node a (Tree a) (Tree a)
  deriving (Show)

inOrderTraversal :: Tree a -> [a]
inOrderTraversal Empty = []
inOrderTraversal (Node val left right) = inOrderTraversal left ++ [val] ++ inOrderTraversal right

swapSubtrees :: Int -> Tree a -> Tree a
swapSubtrees _ Empty = Empty
swapSubtrees k (Node val left right)
  | k == 1 = Node val (swapSubtrees k right) (swapSubtrees k left)
  | otherwise = Node val (swapSubtrees (k - 1) left) (swapSubtrees (k - 1) right)

performSwaps :: Tree a -> [Int] -> [[a]]
performSwaps tree [] = []
performSwaps tree (k:ks) =
  let newTree = swapSubtrees k tree
  in inOrderTraversal newTree : performSwaps newTree ks

readInts :: Int -> IO [Int]
readInts 0 = return []
readInts n = do
  x <- readLn
  xs <- readInts (n - 1)
  return (x:xs)


{- 
module MissingNumbers where

quicksort :: (Ord a) => [a] -> [a]
quicksort [] = []
quicksort (x:xs) = quicksort ys ++ [x] ++ quicksort zs
    where
        ys = [a | a <- xs, a <= x]
        zs = [b | b <- xs, b > x]
        
delete :: Int -> [Int] -> [Int]
delete _ [] = []
delete x (y:ys) 
    | x == y = ys
    | otherwise = x : delete x ys

deleteRepeated :: [Int] -> [Int] -> [Int]
deleteRepeated [] ys = ys  
deleteRepeated (x:xs) ys = deleteRepeated xs (delete x ys)

findMissingNumbers :: [Int] -> [Int] -> [Int]
findMissingNumbers listA listB =
    quicksort $ deleteRepeated listA listB -}