module WordSearch where

import Help.Todo
import System.FilePath ((</>))
import Data.List (elemIndices, isPrefixOf, (!?))

parseInput :: IO [String]
parseInput = lines <$> readFile ("4" </> "input.txt")

getIndices :: [String] -> Char -> [(Int, Int)]
getIndices xs c = concat $ zipWith makeTuples findIndices [0..]
    where findIndices = map (elemIndices c) xs
          makeTuples ys i = map (i,) ys

lookForXMAS :: (Int -> Int) -> (Int -> Int) -> [String] -> (Int, Int) -> Maybe Bool
lookForXMAS fx fy xs (x, y) = liftA2 (==) (sequence [mX, mM, mA, mS]) (Just "XMAS")
    where mX = xs !? x >>= \ys -> ys !? y
          mM = xs !? (fx x) >>= \ys -> ys !? (fy y)
          mA = xs !? (fx . fx $ x) >>= \ys -> ys !? (fy . fy $ y)
          mS = xs !? (fx . fx . fx $ x) >>= \ys -> ys !? (fy . fy . fy $ y)

checkXMAS :: [String] -> (Int, Int) -> Int
checkXMAS xs pos = length . filter ff $ [lookForXMAS (+1) id xs pos,
                                         lookForXMAS (subtract 1) id xs pos,
                                         lookForXMAS id (+1) xs pos,
                                         lookForXMAS id (subtract 1) xs pos,
                                         lookForXMAS (+1) (+1) xs pos,
                                         lookForXMAS (+1) (subtract 1) xs pos,
                                         lookForXMAS (subtract 1) (+1) xs pos,
                                         lookForXMAS (subtract 1) (subtract 1) xs pos]
    where ff (Just b) = b
          ff Nothing  = False

countXMAS :: [String] -> Int
countXMAS xs = foldr (\pos n -> n + checkXMAS xs pos) 0 (getIndices xs 'X')

printResult :: IO ()
printResult = parseInput >>= print . countXMAS
