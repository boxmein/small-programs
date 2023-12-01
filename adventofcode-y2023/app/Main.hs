module Main (main) where

import DayOne (dayOnePartOne)
import Lib (stringReplace)

main :: IO ()
main = dayOnePartOneMain


dayOnePartOneMain :: IO () 
dayOnePartOneMain = do 
  putStrLn "DAY ONE"
  fd <- readFile "./res/day1-input.txt"
  (putStrLn . show . dayOnePartOne . lines) fd
