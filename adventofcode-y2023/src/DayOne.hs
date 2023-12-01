module DayOne (
  dayOnePartOne,
  firstDigit,
  lastDigit
) where 

import Data.Char (isDigit)


dayOnePartOne :: [[Char]] -> Integer
dayOnePartOne = sum . map read . map getFirstAndLastDigit
  
getFirstAndLastDigit :: [Char] -> [Char]
getFirstAndLastDigit s = firstDigit s : lastDigit s : []

digits :: [Char] -> [Char]
digits = filter isDigit

firstDigit :: [Char] -> Char
firstDigit = head . digits

lastDigit :: [Char] -> Char
lastDigit = last . digits
