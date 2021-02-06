module Main where

import Data.List (find)



evenlyDivisibleBy :: Int -> Int -> Bool
evenlyDivisibleBy divisor dividend = (dividend `mod` divisor) == 0

numberFits :: Int -> Bool
numberFits n = evenlyDivisibleBy 1 n &&
               evenlyDivisibleBy 2 n &&
               evenlyDivisibleBy 3 n &&
               evenlyDivisibleBy 4 n &&
               evenlyDivisibleBy 5 n &&
               evenlyDivisibleBy 6 n &&
               evenlyDivisibleBy 7 n &&
               evenlyDivisibleBy 8 n &&
               evenlyDivisibleBy 9 n &&
               evenlyDivisibleBy 10 n &&
               evenlyDivisibleBy 11 n &&
               evenlyDivisibleBy 12 n &&
               evenlyDivisibleBy 13 n &&
               evenlyDivisibleBy 14 n &&
               evenlyDivisibleBy 15 n &&
               evenlyDivisibleBy 16 n &&
               evenlyDivisibleBy 17 n &&
               evenlyDivisibleBy 18 n &&
               evenlyDivisibleBy 19 n &&
               evenlyDivisibleBy 20 n

printRes :: Maybe Int -> IO () 
printRes (Just value) = putStrLn ("Value found: " ++ (show value))
printRes Nothing = return ()


main :: IO () 
main = do 
  putStrLn "trying to find the right one"
  let res = find numberFits [1..]
         in printRes res
