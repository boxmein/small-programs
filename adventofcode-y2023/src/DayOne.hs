module DayOne (
  dayOnePartOne, dayOnePartTwo, 
  getFirstAndLastDigitPartTwo, 
  firstDigitPartTwo, 
  lastDigitPartTwo
) where 

import Data.List (isPrefixOf, isSuffixOf, find)
import Data.Maybe (isJust, fromJust)


dayOnePartOne :: [[Char]] -> Integer
dayOnePartOne = sum . map (read . fromJust) . filter isJust . map getFirstAndLastDigit

dayOnePartTwo :: [[Char]] -> Integer
dayOnePartTwo = sum . map (read . fromJust) . filter isJust . map getFirstAndLastDigitPartTwo


getFirstAndLastDigit :: [Char] -> Maybe [Char]
getFirstAndLastDigit s =
    case (firstDigit s, lastDigit s) of
        (Just fd, Just ld) -> Just (fd : ld : [])
        _ -> Nothing

getFirstAndLastDigitPartTwo :: [Char] -> Maybe [Char]
getFirstAndLastDigitPartTwo s =
    case (firstDigitPartTwo s, lastDigitPartTwo s) of
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

digitWords = map fst mapping

condDigitWord :: (String -> String -> Bool) -> String -> Bool
condDigitWord cond s = any (\x -> x `cond` s) digitWords

startsWithDigitWord :: String -> Bool
startsWithDigitWord = condDigitWord isPrefixOf

endsWithDigitWord :: String -> Bool
endsWithDigitWord = condDigitWord isSuffixOf

parseCondDigitWord :: (String -> String -> Bool) -> String -> Maybe Char
parseCondDigitWord cond s = (fmap snd . find isCondWord) mapping
  where isCondWord = (flip cond) s . fst

parseLeadingDigitWord :: String -> Maybe Char
parseLeadingDigitWord = parseCondDigitWord isPrefixOf

parseTrailingDigitWord :: String -> Maybe Char
parseTrailingDigitWord = parseCondDigitWord isSuffixOf


firstDigitPartTwo :: [Char] -> Maybe Char
firstDigitPartTwo s = 
    if matchesDigit (head s) then Just (head s)
    else if startsWithDigitWord s then parseLeadingDigitWord s
    else firstDigitPartTwo (tail s)

lastDigitPartTwo :: [Char] -> Maybe Char
lastDigitPartTwo s = 
    if matchesDigit (last s) then Just (last s)
    else if endsWithDigitWord s then parseTrailingDigitWord s
    else lastDigitPartTwo (init s)

firstDigit :: [Char] -> Maybe Char
firstDigit (x:xs) = if matchesDigit x then Just x else firstDigit xs

lastDigit :: [Char] -> Maybe Char
lastDigit s = if matchesDigit (last s) then Just (last s) else lastDigit (init s)
