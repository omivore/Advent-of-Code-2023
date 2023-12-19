module Main where

import qualified Solve (parseAll, testItem, ratings, getAllAccepted, scoreAccepted, Result(..), Range(..))

main :: IO ()
main = do
        contents <- readFile "input"
        let (gates, items) = Solve.parseAll contents
        let passed = filter (Solve.testItem gates (Solve.Next "in")) items
        print (sum $ map Solve.ratings passed)

        let initial = [ ("x", Solve.Range 1 4001)
                      , ("m", Solve.Range 1 4001)
                      , ("a", Solve.Range 1 4001)
                      , ("s", Solve.Range 1 4001)
                      ]
        let passes = Solve.getAllAccepted gates (Solve.Next "in") initial
        print (Solve.scoreAccepted passes)
