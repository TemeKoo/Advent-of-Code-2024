module AddMulsConditional where

import Help.Todo
import AddMuls (readInput, addMuls, getMuls)
import Data.List (isPrefixOf)

parseInputDo :: String -> [(Int, Int)]
parseInputDo [] = []
parseInputDo s 
    | "don't()" `isPrefixOf` s = parseInputDont (drop 7 s)
    | "mul(" `isPrefixOf` s = case getMuls (drop 4 s)
                                of Just (x, y) -> (x, y) : parseRest
                                   Nothing -> parseRest
    | otherwise = parseRest
    where parseRest = parseInputDo (tail s)

parseInputDont :: String -> [(Int, Int)]
parseInputDont [] = []
parseInputDont s
    | "do()" `isPrefixOf` s = parseInputDo (drop 4 s)
    | otherwise = parseInputDont (tail s)

printResult :: IO ()
printResult = readInput >>= print . addMuls . parseInputDo
