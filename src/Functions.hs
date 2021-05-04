module Functions where

import Parser (LispVal(..), validSymbols)


{-|
    > isSymbol $ Atom "!"
    True

-}
isSymbol_ :: LispVal -> Bool
isSymbol_ val =
    case val of
        Atom str ->  (head str) `elem` validSymbols
        _ -> False


-- isSymbol :: LispVal -> LispVal
-- isSymbol val =
--     if isSymbol_ val then Atom "#t" else Atom "#f"

isSymbol :: LispVal -> LispVal
isSymbol  = liftL isSymbol_


liftL :: (LispVal -> Bool) -> (LispVal -> LispVal)
liftL f val = if f val then Atom "#t" else Atom "#f"

isAtom_ :: LispVal -> Bool
isAtom_ val =
    case val of
        Atom _ -> True
        _ -> False

{-|

    > eval' "(atom? 'u)"
    Atom "#t"

    > eval' "(atom? 1)"
    Atom "#f"   

-}
isAtom = liftL isAtom_