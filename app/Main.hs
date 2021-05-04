module Main where
    
import System.Environment

import Text.ParserCombinators.Parsec (parse)
import Parser
import Eval



main :: IO ()
main = getArgs >>= print . eval . readExpr' . head


readExpr' :: String -> LispVal
readExpr' input = case parse parseExpr "lisp" input of
    Left err -> String $ "No match: " ++ show err
    Right val -> val