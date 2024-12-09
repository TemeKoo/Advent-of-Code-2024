module ListSimilarity where

import Help.Todo
import ListDistance (parseInput)

import qualified Data.Map as Map


splitLists :: [Int] -> ([Int], [Int])
splitLists [] = ([], [])
splitLists (x:y:xs) = (x:as, y:bs)
    where (as, bs) = splitLists xs

makeMap :: [Int] -> Map.Map Int Int
makeMap = foldr (\x -> Map.insertWith (+) x 1) Map.empty

calculateSimilarity :: [Int] -> Map.Map Int Int -> Int
calculateSimilarity xs m = sum $ map f xs
    where f k = k * Map.findWithDefault 0 k m

printResult :: IO ()
printResult = do
    input <- parseInput
    let (xs, ys) = splitLists input
    print $ calculateSimilarity xs (makeMap ys)
