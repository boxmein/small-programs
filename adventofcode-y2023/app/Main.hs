module Main (main) where

import DayOne (dayOnePartOne, dayOnePartTwo)
import Lib (stringReplace, runOverLinesOfFile)

main :: IO ()
main = dayOnePartTwoMain

dayOnePartOneMain :: IO () 
dayOnePartOneMain = runOverLinesOfFile "./res/day1-input.txt" dayOnePartOne

dayOnePartTwoMain :: IO () 
dayOnePartTwoMain = runOverLinesOfFile "./res/day1-input.txt" dayOnePartTwo
