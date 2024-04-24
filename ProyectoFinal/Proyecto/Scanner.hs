{-# LANGUAGE UndecidableInstances #-}

module Scanner where
import Data.Char (isAlphaNum)
import Data.List (isPrefixOf)
import Data.Char (isSpace)
-- cabal install uulib

type Col = Int

type Line = Int

type Value = String

type Input = String

data Token = Token Type Value Line Col

data Type
  = String
  | OpenBlock
  | EndBlock
  | Keyword
  | EndSlide
  | Photo
  | Error
  | Comment
  deriving (Eq, Ord)

instance Show Token where
  show (Token t v l c) = show t ++ show v ++ " " ++ show l ++ " " ++ show c ++ "\n"

instance Show Type where
  show String = "String: "
  show OpenBlock = "OpenBlock: "
  show EndBlock = "EndBlock: "
  show Keyword = "Keyword: "
  show Photo = "Photo: "
  show Error = "Error: "
  show EndSlide = "EndSlide: "
  -- show Comment = "Comment: "

instance (Eq Type) => (Eq Token) where
  (Token String s1 _ _) == (Token String s2 _ _) = True
  (Token OpenBlock _ _ _) == (Token OpenBlock _ _ _) = True
  (Token EndBlock _ _ _) == (Token EndBlock _ _ _) = True
  (Token Keyword k1 _ _) == (Token Keyword k2 _ _) = k1 == k2
  (Token Photo p1 _ _) == (Token Photo p2 _ _) = p1 == p2
  (Token Error _ _ _) == (Token Error _ _ _) = True
  (Token EndSlide _ _ _) == (Token EndSlide _ _ _) = True
  (Token t1 s1 _ _) == (Token t2 s2 _ _) = t1 == t2 && s1 == s2

instance Ord Token where
  compare x y | x == y = EQ
              | x <= y = LT
              | otherwise = GT
  (Token t1 s1 _ _) <= (Token t2 s2 _ _) = t1 < t2 || (t1 == t2 && s1 <= s2)


scanner :: Input -> [Token]
scanner xs = scan xs 1 1

scan :: Input -> Line -> Col -> [Token]
scan [] _ _ = []
scan ('\n':xs) l _ = scan xs (l+1) 1
scan (' ':xs) l c = scan xs l (c+1)
scan (x:xs) l c
  | x == '!' = scanExclamationMark xs l c
  | x == '#' = 
              let (token, rest) = span (\c -> c == '#') (x:xs)
                  tokenLength = length token
              in Token Keyword token l c : scan rest l (c + tokenLength)
  | x == ';' = scan (dropWhile (/= '\n') xs) (l + 1) 1
  | x == '*' = Token Keyword [x] l c: scan xs l (c+1)
  | x == '\t' = scan xs l (c + 1)
  | x == '\r' = scan xs l (c + 1)
  | x == ' ' = scan xs l (c+1)
  | x == '\n'= scan xs (l+1) 1
  | x == '_' = Token Keyword [x] l c: scan xs l (c+1)
  | x == '+' = 
                let (listItem, rest) = span (== '+') (x:xs)
                    tokenLength = length listItem
                in Token Keyword listItem l c : scan rest l (c + tokenLength)
  | x == '{' = Token OpenBlock [x] l c: scan xs l (c+1)
  | x == '}' = Token EndBlock [x] l c: scan xs l (c+1)
  | x == '<' = 
                let (token, rest) = span (/= '>') xs
                    tokenLength = length token + 2 -- Include both '<' and '>'
                in Token Keyword "<" l c : Token String token l (c+1) : Token Keyword ">" l (c + tokenLength + 1) : scan rest l (c + tokenLength + 2)
  | x == '~' = let (token, rest) = span (\c -> c == '~') (x:xs)
                         in Token Keyword token l c : scan rest l (c + length token)
  | take 3 (x:xs) == "---" = Token EndSlide "---" l c : scan (drop 3 xs) l (c+3)
  | isNumOrSpace x =
      let (token, rest) = span isNumOrSpace (x:xs)
          tokenLength = length token
       in Token String token l c : scan rest l (c + tokenLength)
  | take 3 (x:xs) == "```" = scanCodeBlock xs l c
  | otherwise = Token Error [x] l c : scan xs l (c+1)
  where
    isNumOrSpace :: Char -> Bool
    isNumOrSpace c = c `elem` ":/.?=&%-ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789 "


scanExclamationMark :: Input -> Line -> Col -> [Token]
scanExclamationMark xs l c = Token Keyword "!" l c : scan xs l (c+1)

scanPlus :: Input -> Line -> Col -> [Token]
scanPlus (x:xs) l c =
  let (listItem, rest) = span (== '+') (x:xs)
      tokenLength = length listItem
  in Token Keyword listItem l c : scan rest l (c + tokenLength)

scanCodeBlock :: Input -> Line -> Col -> [Token]
scanCodeBlock xs l c =
  let (code, rest) = getCode xs
      tokenLength = length code
  in Token Keyword "```" l c : Token String code l (c+3) : Token Keyword "```" l (c + tokenLength + 3) : scan rest l (c + tokenLength + 3)
  where
    getCode :: Input -> (Input, Input)
    getCode ('`':'`':'`':rest) = ([], rest)
    getCode ('`':rest) = getCodeSingleBacktick rest
    getCode (x:xs) = let (code, rest) = getCode xs in (x:code, rest)
    
    getCodeSingleBacktick :: Input -> (Input, Input)
    getCodeSingleBacktick ('`':'`':'`':rest) = ([], rest)
    getCodeSingleBacktick (x:xs) = let (code, rest) = getCodeSingleBacktick xs in (x:code, rest)
