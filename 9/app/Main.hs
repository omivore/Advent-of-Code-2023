module Main where

import qualified Solve (cascadeHistories, deriveHistories, toHistory)

main :: IO ()
main = do
        contents <- readFile "input"
        let pipeline = (Solve.cascadeHistories 0)
                       . Solve.deriveHistories
                       . Solve.toHistory
        let futures = map pipeline (lines contents)
        print (sum futures)
