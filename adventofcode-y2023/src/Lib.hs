module Lib
    ( stringReplace
    , runOverLinesOfFile
    ) where

stringReplace :: [Char] -> [Char] -> [Char] -> [Char]
stringReplace haystack needle replacement = 
  let needleLen = (length needle)
      startChars = (take needleLen haystack)
      firstChar = head haystack
      rest = tail haystack
      in
        if (length startChars) < needleLen then
          haystack
        else if startChars == needle then 
          replacement ++ (drop needleLen haystack)
        else 
          firstChar : (stringReplace rest needle replacement)

runOverLinesOfFile :: (Show a) => String -> ([String] -> a) -> IO () 
runOverLinesOfFile filename func = do 
  fd <- readFile filename
  (putStrLn . show . func . lines) fd
