module Main where
import GHC.Compact (getCompact)

-- Input "II + III"
-- Output "V"
-- II - III = -I
-- II * III = VI
-- VI : III = II
-- V % III = II

roman :: [(String, Int)]
roman =
  [ ("I", 1),
    ("IV", 4),
    ("V", 5),
    ("IX", 9),
    ("X", 10),
    ("XL", 40),
    ("L", 50),
    ("XC", 90),
    ("C", 100),
    ("CD", 400),
    ("D", 500),
    ("CM", 900),
    ("M", 1000)
  ]

roman2 = reverse roman

expr :: [(String, Int -> Int -> Int)]
expr = [("+", (+)), ("-", (-)), ("*", (*)), (":", div), ("%", mod)]

callRoman :: String -> String
callRoman input = input ++ " = " ++ result (words input)
  where
    result :: [String] -> String
    result (x : y : z : xs) = numberToRoman (getOperation y (romanToInt x) (romanToInt z))

    getOperation :: String -> Int -> Int -> Int
    getOperation y = snd (head (filter (\n -> fst n == y) expr))

romanToInt :: String -> Int
romanToInt "" = 0
romanToInt (x : xs)
  -- \| length xs > 0 && findNumber (x : head xs : []) == [] =
  | length xs > 0 && getRoman (x : head xs : []) = getNumber (x : head xs : []) + romanToInt (tail xs) -- (x : head xs : [])
  | otherwise = getNumber [x] + romanToInt xs
  where
    getRoman :: String -> Bool
    getRoman x = findNumber x /= []

    findNumber :: String -> [(String, Int)]
    findNumber x = [r | r <- roman, fst r == x]

    getNumber :: String -> Int
    getNumber x = snd $ head $ filter (\n -> fst n == x) roman

numberToRoman :: Int -> String
numberToRoman num = convertToRoman num 0

convertToRoman :: Int -> Int -> String
convertToRoman 0 _ = ""
convertToRoman num pos
  | num >= snd (roman2 !! pos) = fst (roman2 !! pos) ++ convertToRoman (num - snd (roman2 !! pos)) pos
  | otherwise = convertToRoman num (pos + 1)

main :: IO ()
-- Simplificado
main = mapM_ putStrLn . map callRoman . lines =<< getContents
-- main = do 
--    strings <- getContents
--    mapM_ putStrLn $ map callRoman $ lines strings



-- map  f  [a,b,c,d] = [H, H, H, H]- > transforma lo que quieras  es igual a maybe
-- filter f (A) [a,b,c,d,a] = [A, A] -> Buscar el A
-- fokdr f (A) [a,b,c,d] = Z -> transforma esa lista en ese tipo de elementos
-- fokdl f (A) [a,b,c,d] = Z -> es igual al maybe y se resolvia con filter.
-- 