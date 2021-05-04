module Eval where

import Text.ParserCombinators.Parsec hiding (spaces)

import Parser
    ( LispVal(Atom, String, Number, Bool, List), parseExpr, run )  

import Functions


{-| 

    > eval' "(+ 1 2)"
    3

    > eval' "(symbol? '!)"
    Atom "#t"

    > eval' "(symbol? 3)"
    Atom "#f"
    
-}
eval' :: String -> LispVal
eval' input = case parse parseExpr "lisp" input of
    Left err -> String $ "No match: " ++ show err
    Right val -> eval val

-- FUNCTION APPLICATION


apply :: String -> [LispVal] -> LispVal
apply func args = maybe (Bool False) ($ args) $ lookup func primitives

primitives :: [(String, [LispVal] -> LispVal)]
primitives = [("+", numericBinop (+)),
              ("-", numericBinop (-)),
              ("*", numericBinop (*)),
              ("/", numericBinop div),
              ("mod", numericBinop mod),
              ("quotient", numericBinop quot),
              ("remainder", numericBinop rem),
              ("symbol?", Functions.isSymbol . head),
              ("atom?", Functions.isAtom . head)

              ]

numericBinop :: (Integer -> Integer -> Integer) -> [LispVal] -> LispVal
numericBinop op params = Number $ foldl1 op $ map unpackNum params

unpackNum :: LispVal -> Integer
unpackNum (Number n) = n
unpackNum (String n) = let parsed = reads n :: [(Integer, String)] in 
                           if null parsed 
                              then 0
                              else fst $ parsed !! 0
unpackNum (List [n]) = unpackNum n
unpackNum _ = 0


-- BASICS: 
eval :: LispVal -> LispVal
eval val@(String _) = val
eval val@(Number _) = val
eval val@(Bool _) = val
eval (List [Atom "quote", val]) = val
eval (List (Atom func : args)) = apply func $ map eval args
eval _ = Atom "Error"


