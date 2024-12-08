module ListDistance where

import Help.Todo
import System.FilePath
import Control.Applicative

parseInput :: IO [Int]
parseInput = readFile ("1" </> "input.txt") >>=
             \s  -> return . fmap read $ words s

printInput :: IO ()
printInput = parseInput >>= print . length
