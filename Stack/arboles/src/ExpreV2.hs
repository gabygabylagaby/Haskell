module ExpreV2 where
import Data.Char (isDigit)

data Expression = Term Term
                | Sub Term Expression
                | Add Term Expression
                deriving (Show)

data Term = Factor Factor
            | Div Factor Term
            | Mul Factor Term
            deriving (Show)

data Factor = Lit Integer
            | Negative Factor
            | Parenthesis Expression
            deriving (Show)

-- 1000000007
modulo :: Integer
modulo = 10^9 + 7

-- Me ayuda a evaluar la expresion
evalExpression :: Expression -> Integer
evalExpression (Term t) = evaluateTerm t
evalExpression (Add t e) = (evaluateTerm t + evalExpression e) `mod` modulo
evalExpression (Sub t e) = (evaluateTerm t - evalExpression e) `mod` modulo

-- Me ayuda a evaluar el termino
evaluateTerm :: Term -> Integer
evaluateTerm (Factor f) = evaluateFactor f
evaluateTerm (Mul f t) = (evaluateFactor f * evaluateTerm t) `mod` modulo
evaluateTerm (Div f t) = (evaluateFactor f * pow (evaluateTerm t) (modulo - 2)) `mod` modulo

-- Me ayuda a evaluar el factor
evaluateFactor :: Factor -> Integer
evaluateFactor (Lit n) = n
evaluateFactor (Negative f) = (- evaluateFactor f) `mod` modulo
evaluateFactor (Parenthesis expr) = evalExpression expr

-- Me ayuda a elevar un numero a una potencia
pow :: Integer -> Integer -> Integer
pow _ 0 = 1
pow x n | even n = (pow (x * x `mod` modulo) (n `div` 2)) `mod` modulo
        | otherwise = (x * pow x (n - 1)) `mod` modulo

-- Me ayuda a parsear una expresion
parseExpression :: String -> Expression
parseExpression = fst . parseExp

parseExp :: String -> (Expression, String)
parseExp str =
  let (term, rest) = parseTerm str
  in case rest of
    ('+':xs) -> let (expr, rest') = parseExp xs in (Add term expr, rest')
    ('-':xs) -> let (expr, rest') = parseExp xs in (Sub term expr, rest')
    _        -> (Term term, rest)

parseTerm :: String -> (Term, String)
parseTerm str =
  let (factor, rest) = parseFactor str
  in case rest of
    ('*':xs) -> let (term, rest') = parseTerm xs in (Mul factor term, rest')
    ('/':xs) -> let (term, rest') = parseTerm xs in (Div factor term, rest')
    _        -> (Factor factor, rest)

parseFactor :: String -> (Factor, String)
parseFactor ('(':rest) = let (expr, rest') = parseExp rest in (Parenthesis expr, rest')
parseFactor ('-':rest) = let (factor, rest') = parseFactor rest in (Negative factor, rest')
parseFactor str = (Lit (read num), rest)
  where (num, rest) = span isDigit (dropWhile (== ' ') str)

main :: IO ()
main = do
    let examples = ["22*79-21", "55+3-45*33-25", "4/-2/(2+8)",  "4/-2/2+8"]
    let results = map (\exp -> evalExpression (parseExpression exp)) examples
    mapM_ print results

