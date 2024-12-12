module CountSafe where

import Help.Todo
import System.FilePath

parseInput :: IO [[Int]]
parseInput = readFile ("2" </> "input.txt") >>=
             \s  -> return . fmap (fmap read . words) $ lines s

checkIncreasing :: [Int] -> Bool
checkIncreasing [_] = True
checkIncreasing (x:y:xs) = (y-x `elem` [1,2,3]) && checkIncreasing (y:xs)

checkDecreasing :: [Int] -> Bool
checkDecreasing [_] = True
checkDecreasing (x:y:xs) = (x-y `elem` [1,2,3]) && checkDecreasing (y:xs)

checkSafe :: [Int] -> Bool
checkSafe (x:y:xs) | x > y = checkDecreasing (x:y:xs)
                   | x < y = checkIncreasing (x:y:xs)
                   | otherwise = False

countSafe :: [[Int]] -> Int
countSafe = foldr (\ys -> (+) (if checkSafe ys then 1 else 0)) 0

printResult :: IO ()
printResult = parseInput >>= print . countSafe
