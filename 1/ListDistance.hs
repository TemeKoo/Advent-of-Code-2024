module ListDistance where

import Help.Todo
import System.FilePath
import Data.List

parseInput :: IO [Int]
parseInput = readFile ("1" </> "input.txt") >>=
             \s  -> return . fmap read $ words s

sortInput :: [Int] -> [(Int, Int)]
sortInput [] = []
sortInput xs = zip (sort $ as xs) (sort $ bs xs)
    where as [] = []
          as (x:_:rs) = x:as rs
          bs [] = []
          bs (_:y:rs) = y:bs rs

calculateDistances :: [(Int, Int)] -> Int
calculateDistances = sum . map (\(a,b) -> abs (a-b))

printResult :: IO ()
printResult = parseInput >>= print . calculateDistances . sortInput
