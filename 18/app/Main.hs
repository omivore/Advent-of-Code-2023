module Main where

import qualified Solve (countArea, parseLine1, parseLine2)

main :: IO ()
main = do
        contents <- readFile "input"
        print $ Solve.countArea Solve.parseLine1 contents
        print $ Solve.countArea Solve.parseLine2 contents
