module Solve where

splitOn :: Char -> String -> [String]
splitOn split input = [takeWhile isSplit input,
                       tail (dropWhile isSplit input)]
                      where isSplit = (/=) split

getBounds :: String -> [Integer]
getBounds assign = [read (head bounds), read (last bounds)]
                   where bounds = splitOn '-' assign

expand :: [Integer] -> [Integer]
expand assign = [(head assign)..(last assign)]

disjoint :: [Integer] -> [Integer] -> [[Integer]]
disjoint xs@(x:xs') ys@(y:ys')
         | x < y  = prependl x (disjoint xs' ys)
         | x == y = disjoint xs' ys'
         | x > y  = prependr y (disjoint xs ys')
         where
           prependl l assign = [l : head assign, last assign]
           prependr r assign = [head assign, r : last assign]
disjoint xs [] = [xs, []]
disjoint [] ys = [[], ys]

joint :: [Integer] -> [Integer] -> [Integer]
joint xs@(x:xs') ys@(y:ys')
      | x < y  = joint xs' ys
      | x == y = x : joint xs' ys'
      | x > y  = joint xs ys'
joint xs [] = []
joint [] ys = []

overlaps :: [[Integer]] -> Bool
overlaps assign = not (null (joint (head assign) (last assign)))

fullyContains :: [[Integer]] -> Bool
fullyContains assign = null (head leftovers) || null (last leftovers)
                       where leftovers = disjoint (head assign) (last assign)
