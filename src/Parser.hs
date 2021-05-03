module Parser where

-- https://en.wikibooks.org/wiki/Write_Yourself_a_Scheme_in_48_Hours/Parsing
-- https://www.schoolofhaskell.com/school/to-infinity-and-beyond/pick-of-the-week/parsing-floats-with-parsec

import Text.ParserCombinators.Parsec hiding (spaces)
import Control.Monad
import System.Environment

data LispVal = Atom String
             | List [LispVal]
             | DottedList [LispVal] LispVal
             | Number Integer
             | String String
             | Bool Bool
    deriving Show


parseString :: Parser LispVal
parseString = do
                char '"'
                x <- many (noneOf "\"")
                char '"'
                return $ String x

parseAtom :: Parser LispVal
parseAtom = do 
              first <- letter <|> symbol
              rest <- many (letter <|> digit <|> symbol)
              let atom = first:rest
              return $ case atom of 
                         "#t" -> Bool True
                         "#f" -> Bool False
                         _    -> Atom atom


parseNumber :: Parser LispVal
parseNumber = liftM (Number . read) $ many1 digit

parseNumber2 :: Parser LispVal
parseNumber2 = do 
      digits <- many1 digit
      return (Number (read digits))


parseExpr :: Parser LispVal
parseExpr = parseAtom
         <|> parseString
         <|> parseNumber

symbol :: Parser Char
symbol = oneOf "!#$%&|*+-/:<=>?@^_~"

spaces :: Parser ()
spaces = skipMany1 space

readExpr :: String -> String
readExpr input = case parse parseExpr "lisp" input of
    Left err -> "No match: " ++ show err
    Right val -> "Found value: "  ++ show val



