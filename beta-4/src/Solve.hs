module Solve where

import Data.Bifunctor

type Section = Integer
type Bounds = (Section, Section)

mapToPair f = bimap f f

splitOn :: Char -> String -> (String, String)
splitOn split input = (takeWhile isSplit input,
                       tail (dropWhile isSplit input))
                      where isSplit = (/= split)

getBounds :: String -> Bounds
getBounds assign = mapToPair read (splitOn '-' assign)

expand :: Bounds -> [Section]
expand bounds = [(fst bounds)..(snd bounds)]

disjoint :: [Section] -> [Section] -> ([Section], [Section])
disjoint xs@(x:xs') ys@(y:ys')
         | x < y  = first (x :) (disjoint xs' ys)
         | x == y = disjoint xs' ys'
         | x > y  = second (y :) (disjoint xs ys')
disjoint xs [] = (xs, [])
disjoint [] ys = ([], ys)

joint :: [Section] -> [Section] -> [Section]
joint xs@(x:xs') ys@(y:ys')
      | x < y  = joint xs' ys
      | x == y = x : joint xs' ys'
      | x > y  = joint xs ys'
joint xs [] = []
joint [] ys = []

overlaps :: ([Section], [Section]) -> Bool
overlaps assign = not (null (uncurry joint assign))

fullyContains :: ([Section], [Section]) -> Bool
fullyContains assign = null (fst leftovers) || null (snd leftovers)
                       where leftovers = uncurry disjoint assign
