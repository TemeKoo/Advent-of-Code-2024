module CountSafeDampened where

import Help.Todo

import CountSafe (parseInput, checkSafe)

dropAtIndex :: Int -> [a] -> [a]
dropAtIndex i xs = take i xs ++ drop (i+1) xs

checkSafeDamped :: [Int] -> Int ->  Bool
checkSafeDamped xs i 
    | i < length xs = checkSafe (dropAtIndex i xs) || checkSafeDamped xs (i+1)
    | otherwise = False

countSafeDampened :: [[Int]] -> Int
countSafeDampened = foldr (\xs -> (+) (if f xs then 1 else 0)) 0
    where f xs = checkSafe xs || checkSafeDamped xs 0

printResult :: IO ()
printResult = parseInput >>= print . countSafeDampened
