module Expression where

data Exp = Lit Integer
         | Add Exp Exp
         | Mul Exp Exp
         | Sub Exp Exp
         | Div Exp Exp  
         | If Cond Exp Exp
         | For Cond Exp Exp Exp
         deriving (Eq)

data  Cond = Eq Exp Exp
           | Lt Exp Exp -- Less than
           | Gt Exp Exp -- Greater than
           | Not Cond
           deriving (Eq)

instance Show Exp where
  show (Lit n) = show n
  show (Add a b) = par (show a ++ " + " ++ show b)
  show (Mul a b) = par (show a ++ " * " ++ show b)
  show (Sub a b) = par (show a ++ " - " ++ show b)
  show (Div a b) = par (show a ++ " / " ++ show b)
  show (If c a b) = "if " ++ show c ++ " then " ++ show a ++ " else " ++ show b
  show (For c a b e) = "for" ++ show c ++ " in " ++ show a ++ " to " ++ show b ++ " do " ++ show e

-- Cada condiciom o expresion que pongas em Exp, se debe mostrar en show, para imprimirlo.

instance Show Cond where
  show (Eq a b) = show a ++ " == " ++ show b
  show (Lt a b) = show a ++ " < " ++ show b
  show (Gt a b) = show a ++ " > " ++ show b
  show (Not c) = "not " ++ show c

evalExp :: Exp -> Integer
evalExp (Lit n) = n
evalExp (Add a b) = evalExp a + evalExp b
evalExp (Mul a b) = evalExp a * evalExp b
evalExp (Sub a b) = evalExp a - evalExp b
evalExp (Div a b) = evalExp a `div` evalExp b
evalExp (If c a b) = if evalCond c then evalExp a else evalExp b

evalCond :: Cond -> Bool
evalCond (Eq a b) = evalExp a == evalExp b
evalCond (Lt a b) = evalExp a < evalExp b
evalCond (Gt a b) = evalExp a > evalExp b

par :: String -> String 
par x = "(" ++ x ++ ")"

ejemplo0 = Add (Lit 1) (Mul (Lit 2) (Lit 3))
ejemplo1 = Mul (Add (Lit 1) (Lit 2)) (Lit 3)
ejemplo2 = Add ejemplo0 (Mul (Lit 3) ejemplo1) 

c0 = Lt ejemplo0 ejemplo2
ejemplo3 = If c0 ejemplo0 ejemplo2

{-
Detectar la estructura de un codigo 
- Tokenizar
- Detectar todo un codigo

-}