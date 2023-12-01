module Main (main) where

import DayOne

main :: IO ()
main = do 
  putStrLn "DAY ONE"
  fd <- readFile "./res/day1-part1-input.txt"
  (putStrLn . show . dayOnePartOne . lines) fd
  

