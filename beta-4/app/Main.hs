module Main where

import qualified Solve (splitOn, getBounds, expand, disjoint, joint, overlaps, fullyContains, mapToPair)

main :: IO ()
main = do
        contents <- readFile "input"
        let pipeline1 = Solve.fullyContains
                      . Solve.mapToPair (Solve.expand . Solve.getBounds)
                      . Solve.splitOn ','
        let contains = map pipeline1 (lines contents)
        print (sum $ map fromEnum contains)

        let pipeline2 = Solve.overlaps
                      . Solve.mapToPair (Solve.expand . Solve.getBounds)
                      . Solve.splitOn ','
        let overlaps = map pipeline2 (lines contents)
        print (sum $ map fromEnum overlaps)
