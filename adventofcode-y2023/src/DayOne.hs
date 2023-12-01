module DayOne (
  dayOnePartOne
) where 


dayOnePartOne :: [[Char]] -> Integer
dayOnePartOne = sum . map (read . getFirstAndLastDigit)

getFirstAndLastDigit :: [Char] -> [Char]
getFirstAndLastDigit s = firstDigit s : lastDigit s : []

matchesDigit :: Char -> Bool
matchesDigit x = x `elem` ['0'..'9']

firstDigit :: [Char] -> Char
firstDigit (x:xs) = if matchesDigit x then x else firstDigit xs

lastDigit :: [Char] -> Char
lastDigit s = 
  let 
      x = last s  
      xs = init s
  in
    if matchesDigit x then x else lastDigit xs
