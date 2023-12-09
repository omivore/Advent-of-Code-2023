module Main where

import qualified Solve (cascadeHistories, cascadeFutures, deriveHistories, toHistory)

main :: IO ()
main = do
        contents <- readFile "input"
        let futurePipeline = (Solve.cascadeHistories 0)
                             . Solve.deriveHistories
                             . Solve.toHistory
        let futures = map futurePipeline (lines contents)
        print (sum futures)

        let pastPipeline = (Solve.cascadeFutures 0)
                           . Solve.deriveHistories
                           . Solve.toHistory
        let pasts = map pastPipeline (lines contents)
        print (sum pasts)
