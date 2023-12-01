module DayOne (
  dayOnePartOne
) where 

import Data.List (isPrefixOf, isSuffixOf, find)
import Data.Maybe (isJust, fromJust)


dayOnePartOne :: [[Char]] -> Integer
dayOnePartOne inputLines = 
  let linesMaybes = map getFirstAndLastDigit inputLines
      linesOnlyJusts = filter isJust linesMaybes 
      unwrappedJusts = map fromJust linesOnlyJusts
      parsedInts = map read unwrappedJusts
      summed = sum parsedInts 
    in summed

getFirstAndLastDigit :: [Char] -> Maybe [Char]
getFirstAndLastDigit s =
    case (firstDigit s, lastDigit s) of
        (Just fd, Just ld) -> Just (fd : ld : [])
        _ -> Nothing

matchesDigit :: Char -> Bool
matchesDigit x = x `elem` ['0'..'9']

mapping = [("one", '1'),
  ("two", '2'),
  ("three", '3'),
  ("four", '4'),
  ("five", '5'),
  ("six", '6'),
  ("seven", '7'),
  ("eight", '8'),
  ("nine", '9'),
  ("zero", '0')]

digitWords = map (\(a,b) -> a) mapping

condDigitWord :: (String -> String -> Bool) -> String -> Bool
condDigitWord cond s = any (\x -> x `cond` s) digitWords

startsWithDigitWord :: String -> Bool
startsWithDigitWord = condDigitWord isPrefixOf

endsWithDigitWord :: String -> Bool
endsWithDigitWord = condDigitWord isSuffixOf

parseCondDigitWord :: (String -> String -> Bool) -> String -> Maybe Char
parseCondDigitWord cond s = 
  let 
      isCondWord (word,_) = word `cond` s
      firstMatch = find isCondWord mapping
      takeSecond (first,second) = second
      secondItem = fmap takeSecond firstMatch 
    in 
      secondItem

parseLeadingDigitWord :: String -> Maybe Char
parseLeadingDigitWord = parseCondDigitWord isPrefixOf

parseTrailingDigitWord :: String -> Maybe Char
parseTrailingDigitWord = parseCondDigitWord isSuffixOf


firstDigitPart2 :: [Char] -> Maybe Char
firstDigitPart2 s = 
    if matchesDigit (head s) then Just (head s)
    else if startsWithDigitWord s then parseLeadingDigitWord s
    else firstDigit (tail s)

lastDigitPart2 :: [Char] -> Maybe Char
lastDigitPart2 s = 
    if matchesDigit (head s) then Just (head s)
    else if endsWithDigitWord s then parseTrailingDigitWord s
    else lastDigit (tail s)

firstDigit :: [Char] -> Maybe Char
firstDigit (x:xs) = if matchesDigit x then Just x else firstDigit xs

lastDigit :: [Char] -> Maybe Char
lastDigit s = 
  let 
      x = last s  
      xs = init s
  in
    if matchesDigit x then Just x else lastDigit xs
