module Main (main) where

import DayOne (dayOnePartOne, dayOnePartTwo)
import Lib (runOverLinesOfFile)

main :: IO ()
main = do 
  putStrLn "day 1 advent of code"
  putStrLn "part 1"
  dayOnePartOneMain
  putStrLn "part 2"
  dayOnePartTwoMain

dayOnePartOneMain :: IO () 
dayOnePartOneMain = runOverLinesOfFile "./res/day1-input.txt" dayOnePartOne

dayOnePartTwoMain :: IO () 
dayOnePartTwoMain = runOverLinesOfFile "./res/day1-input.txt" dayOnePartTwo
