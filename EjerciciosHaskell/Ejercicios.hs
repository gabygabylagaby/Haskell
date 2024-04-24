
problema1 = [10, 2]
problema2 = [1, 6]

esPrimo :: Int -> Bool
esPrimo 1 = False
esPrimo n = esPrimoAux n 2

esPrimoAux :: Int -> Int -> Bool
esPrimoAux n divisor 
    | divisor * divisor > n = True
    | mod n divisor == 0 = False
    | otherwise = esPrimoAux n (divisor + 1)

sieve :: Int -> [Int]
sieve n = sieve' [2..n] []

sieve' :: [Int] -> [Int] -> [Int]
sieve' [] primes = primes
sieve' (x:xs) primes
    | x * x > last (x:xs) = primes ++ (x:xs)
    | otherwise = sieve' (filter (\y -> y `mod` x /= 0) xs) (primes ++ [x])


main = do
    let primesUpTo100 = sieve 100
    print primesUpTo100


