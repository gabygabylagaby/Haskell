module Main where
import UU.Parsing
import Parser
import Scanner(scanner)

main :: IO ()
main = do input <- readFile "slide.p5"
          let token = scanner input
          putStrLn(show token) 
          tree <- parseIO pSlides token
          putStrLn (show tree)

