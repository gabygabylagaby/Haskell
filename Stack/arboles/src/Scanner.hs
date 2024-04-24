module Scanner where

type Col = Int 
type Line = Int
type Value = String

data Token = Token Type Value Line Col

data Type = String | OpenBlock | EndBlock | EndSlide | Keyword | Error  deriving (Eq, Show) 

type Input = String 

instance Show Token where
  show (Token t v l c) = show t ++ " " ++ show v ++ " " ++ show l ++ " " ++ show c
  

scanner :: Input -> [Token]
scanner xs = scan xs 1 1

scan :: Input -> Line -> Col -> [Token]
scan [] _ _ = []
scan (x:xs) l c 
  | x == '!' = Token Keyword [x] l c : scan xs l (c + 1)
  | x == ' ' = scan xs l (c + 1)
  | x == '\n' = scan xs (l + 1) 1


  
