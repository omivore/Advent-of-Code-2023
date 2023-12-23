module Solve where

import Numeric

type Point = (Float, Float)
type Edge = (Point, Point)
type Dims = (Point, Point)
data State = On | Cross Bool deriving (Eq, Show)

addLine :: Integer -> Integer -> [(Char, Integer)] -> Integer
addLine area _ [] = area
addLine area x ((dir, length):instructions)
    | dir == 'U' = addLine (area - (x * length)) x instructions
    | dir == 'D' = addLine (area + ((x + 1) * length)) x instructions
    | dir == 'L' = addLine area (x - length) instructions
    | dir == 'R' = addLine (area + length) (x + length) instructions
    | otherwise  = error "direction not recognized"

splitOn :: (Char -> Bool) -> String -> [String]
splitOn p s = case dropWhile p s of
                "" -> []
                s' -> w : splitOn p s''
                    where (w, s'') = break p s'

parseLine1 :: String -> (Char, Integer)
parseLine1 line =
    let [[dir], lengthText, _] = splitOn (== ' ') line
        length = read lengthText
    in (dir, length)

parseLine2 :: String -> (Char, Integer)
parseLine2 line =
    let contents = tail $ init $ dropWhile (/= '#') line
        (lengthHex, dirText) = (init contents, last contents)
        dir = case dirText of
            '0' -> 'R'
            '1' -> 'D'
            '2' -> 'L'
            '3' -> 'U'
            _   -> error "direction not recognized"
        [(length, _)] = readHex lengthHex
    in (dir, length)

countArea :: (String -> (Char, Integer)) -> String -> Integer
countArea f input =
    let instructions = map f (lines input)
    in addLine 1 0 instructions
