module Level1
    ( level1Implementation
    ) where

import System.IO (readFile)
import Data.List.Split (splitOn)
import Data.List (elemIndex)

level1Implementation :: IO ()
level1Implementation = do
    lines <- linesOfFile "./input/level1-1.txt"
    let 
        lineBatches = makeBatches lines 
        elfCalories = map sumBatch lineBatches 
        maxCalories = maximum elfCalories
        elfIndex = elemIndex maxCalories elfCalories
        in 
            putStrLn (show maxCalories)

makeBatches = splitOn [""]


sumBatch :: [String] -> Int
sumBatch batch = foldl (+) 0 batchInts 
    where batchInts = map read batch 


linesOfFile :: FilePath -> IO [String]
linesOfFile filename = do
    contents <- readFile filename 
    return (lines contents)