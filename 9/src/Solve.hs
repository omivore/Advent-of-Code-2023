module Solve where

import Data.Bifunctor

type History = [Integer]

toHistory :: String -> History
toHistory input = map read (words input)

differential :: History -> History
differential [] = []
differential history = zipWith (-) (tail history) history

stepHistory :: Integer -> History -> Integer
stepHistory next history = (last history) + next

allZeroes :: History -> Bool
allZeroes [] = False
allZeroes [x] = x == 0
allZeroes (x:xs) = allZeroes [x] && allZeroes xs

deriveHistories :: History -> [History]
deriveHistories history
                | allZeroes history = [history]
                | otherwise         = history : deriveHistories (differential history)

cascadeHistories :: Integer -> [History] -> Integer
cascadeHistories next [topHistory] = stepHistory next topHistory
cascadeHistories next histories = cascadeHistories (stepHistory next (last histories)) (init histories)

stepFuture :: Integer -> History -> Integer
stepFuture next (future:_) = future - next

cascadeFutures :: Integer -> [History] -> Integer
cascadeFutures next [topFuture] = stepFuture next topFuture
cascadeFutures next futures = cascadeFutures (stepFuture next (last futures)) (init futures)
