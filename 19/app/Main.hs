module Main where

import qualified Solve (parseAll, testItem, ratings, Result(..))

main :: IO ()
main = do
        contents <- readFile "input"
        let (gates, items) = Solve.parseAll contents
        let passed = filter (Solve.testItem gates (Solve.Next "in")) items
        print (sum $ map Solve.ratings passed)
