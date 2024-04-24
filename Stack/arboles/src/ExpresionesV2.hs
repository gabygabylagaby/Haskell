module Expression where
import Data.Char (isDigit)

data Exp = Lit Integer
                 | Add Exp Exp
                 | Mul Exp Exp
                 | Sub Exp Exp
                 | Div Exp Exp
                 | OperationUnitary Exp
                 deriving (Eq)

data Expression = Exp
                 | ExpressionAdd Exp Expression
                 | ExpressionSub Exp Expression
                 deriving (Eq)

data Term = Factor Exp
                 | TermMul Exp Term
                 | TermDiv Exp Term
                 deriving (Eq)

data Factor = Number Integer
                 | FactorExp Exp
                 deriving (Eq)

instance Show Exp where
    show (Lit n) = show n
    show (Add a b) = par (show a ++ " + " ++ show b)
    show (Mul a b) = par (show a ++ " * " ++ show b)
    show (Sub a b) = par (show a ++ " - " ++ show b)
    show (Div a b) = par (show a ++ " / " ++ show b)
    show (OperationUnitary a) = par ("-" ++ show a)

instance Show Expression where
    show (Exp) = ""
    show (ExpressionAdd a b) = show a ++ " + " ++ show b
    show (ExpressionSub a b) = show a ++ " - " ++ show b

instance Show Term where
    show (Factor a) = show a
    show (TermMul a b) = show a ++ " * " ++ show b
    show (TermDiv a b) = show a ++ " / " ++ show b

instance Show Factor where
    show (Number n) = show n
    show (FactorExp a) = show a

instance Num Exp where
    a + b = Add a b
    a * b = Mul a b
    a - b = Sub a b
    abs a = OperationUnitary a
    signum a = error "signum no implementado"
    fromInteger n = Lit n
    

evalExp :: Exp -> Integer
evalExp (Lit n) = n
evalExp (Add a b) = evalExp a + evalExp b
evalExp (Mul a b) = evalExp a * evalExp b
evalExp (Sub a b) = evalExp a - evalExp b
evalExp (Div a b) = evalExp a `div` evalExp b
evalExp (OperationUnitary a) = (- evalExp a)

parserExp3 :: String -> Exp
parserExp3 [] = Lit 0
parserExp3 xs
    | all isDigit xs = Lit (read xs)
    | otherwise = case break (`elem` "+-") xs of
            (a, '+':b) -> Add (parserExp3 a) (parserExp3 b)
            (a, '-':b) -> Sub (parserExp3 a) (parserExp3 b)
            _ -> case break (`elem` "*/") xs of
                    (a, '*':b) -> Mul (parserExp3 a) (parserExp3 b)
                    (a, '/':b) -> Div (parserExp3 a) (parserExp3 b)
                    _ -> error "Invalid expression"

par :: String -> String 
par x = "(" ++ x ++ ")"

evalExpMod :: Exp -> Integer -> Integer
evalExpMod expr m = modulo (evalExp expr) m -- 

modulo :: Integer -> Integer -> Integer
modulo x m = (x `mod` m + m) `mod` m

ejemplo0 = "22*79-21"
ejemplo1 = "4/-2/2+8"
ejemplo2 = "55+3-45*33-25"
ejemplo3 = "4/-2/(2+8)"

main :: IO ()
main = do
    let ejemplos = ["22*79-21", "55+3-45*33-25", "4/-2/(2+8)",  "4/-2/2+8"]
    let resultados = map (\exp -> evalExpMod (parserExp3 exp) 1000000007) ejemplos
    mapM_ print resultados

-- parserExp3 "22*79-21"
-- evalExp $ parserExp3 "22*79-21"
-- esto debe salir 1717
-- parserExp3 "4/-2/2+8"
-- (4 / ((-2) / 2)) + 8
-- evalExp $ parserExp3 "4/-2/2+8"
-- Esto debe de salir 4
-- parserExp3 "55+3-45*33-25"
-- evalExp $ parserExp3 "55+3-45*33-25"
-- parserExp3 "55+3-(45*33)-25"
-- 
-- parserExp3 "4/-2/(2+8)"
-- evalExp $ parserExp3 "4/-2/(2+8)"
-- parserExp3 "4/((-2)/(2+8))"
