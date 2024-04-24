module Data where



-- data Boolean = True | False

type Pos = (Int, Int)
data Move = North | South| East| West | Aleatorio 

move :: Move -> Pos -> Pos
move North (x,y) = (x, y+1)
move South (x,y) = (x, y-1)
move East (x,y) = (x+1, y)
move West (x,y) = (x-1, y)
move Aleatorio (x,y) = (x*x, y*y)

data Shape = Circle Float | Rect Float Float 

square:: Float -> Shape 
square n = Rect n n

area :: Shape -> Float
area (Circle r) = pi * r^2
area (Rect x y) = x * y

{- data MyMaybe a = Nothing
            | Just a 
            deriving (show)

-}

data MyMaybe a = NoHay
            | Existe a
            deriving (Show)

safediv :: Int -> Int -> Maybe Int
safediv _ 0 = Nothing
safediv m n = Just (m `div` n)

getValue :: Maybe Int -> Int
getValue Nothing = 0
getValue (Just a) = a

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

  -- Maybe en java es como un 
