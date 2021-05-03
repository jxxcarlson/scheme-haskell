module Main where
    
import System.Environment

-- main :: IO ()
-- main = do
--  args <- getArgs
--  let a = read (args !! 0) :: Int
--  let b = read (args !! 1) :: Int
--  let sum = a + b
--  putStrLn ("The sum is " ++ show sum)

main :: IO ()
main = do
 (arg1:arg2:_) <- getArgs
 let a = read arg1 :: Int
 let b = read arg2 :: Int
 -- let sum = a + b
 putStrLn ("The sum is " ++ (show $ a + b))

