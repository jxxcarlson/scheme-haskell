module Main where
    
import System.Environment
import Parser

main :: IO ()
main = do 
         line <- getLine
         putStrLn (readExpr line)


