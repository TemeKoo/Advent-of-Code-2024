module AddMuls where

import Help.Todo
import System.FilePath ((</>))
import Data.List (isPrefixOf)
import Data.Char (isDigit)
import Text.Read (readMaybe)
import Control.Monad ((>=>))

readInput :: IO String
readInput = readFile ("3" </> "input.txt")

dropNum :: String -> Maybe String
dropNum s = takeNum s >> Just (dropWhile isDigit s)

takeNum :: String -> Maybe Int
takeNum = readMaybe . takeWhile isDigit

dropComma :: String -> Maybe String
dropComma (',':rest) = Just rest
dropComma _ = Nothing

isBracket :: String -> Maybe Bool
isBracket (')':_) = Just True
isBracket _ = Nothing

getMuls :: String -> Maybe (Int, Int)
getMuls s0 = takeNum s0 >>=
        \x  -> (dropNum >=> dropComma) s0 >>=
        \s1 -> (dropNum >=> isBracket) s1 >>
              takeNum s1 >>=
        \y -> Just (x, y)

parseInput :: String -> [(Int, Int)]
parseInput [] = []
parseInput s
    | "mul(" `isPrefixOf` s =
        case getMuls (drop 4 s)
        of Just (x,y) -> (x,y) : parseInput (tail s)
           Nothing -> parseInput (tail s)
    | otherwise = parseInput (tail s)

addMuls :: [(Int, Int)] -> Int
addMuls = sum . map (uncurry (*))

printResult :: IO ()
printResult = readInput >>= print . addMuls . parseInput
