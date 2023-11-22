module Main (main) where

import Test.HUnit
import Test.Framework
import Test.Framework.Providers.HUnit
import Solve (splitOn, getBounds, expand, disjoint, joint, overlaps, fullyContains)

main :: IO ()
main = defaultMain $ hUnitTestToTests $ TestList [
  "input strings split correctly into pairs" ~: do
    (dt, expected) <-
      [
        ("2-4,6-8", ["2-4", "6-8"]),
        ("2-3,4-5", ["2-3", "4-5"]),
        ("5-7,7-9", ["5-7", "7-9"]),
        ("2-8,3-7", ["2-8", "3-7"]),
        ("6-6,4-6", ["6-6", "4-6"]),
        ("2-6,4-8", ["2-6", "4-8"])
      ]
    let actual = splitOn ',' dt
    return $ expected ~=? actual
  ,
  "assignment strings correctly parsed into integer bounds" ~: do
    (dt, expected) <-
      [
        ("2-4", [2, 4]),
        ("2-3", [2, 3]),
        ("5-7", [5, 7]),
        ("2-8", [2, 8]),
        ("6-6", [6, 6]),
        ("2-6", [2, 6]),
        ("6-8", [6, 8]),
        ("4-5", [4, 5]),
        ("7-9", [7, 9]),
        ("3-7", [3, 7]),
        ("4-6", [4, 6]),
        ("4-8", [4, 8])
      ]
    let actual = getBounds dt
    return $ expected ~=? actual
  ,
  "expand bounds into actual arrays of numbers" ~: do
    (dt, expected) <-
      [
        ([2, 4], [2..4]),
        ([2, 3], [2..3]),
        ([5, 7], [5..7]),
        ([2, 8], [2..8]),
        ([6, 6], [6..6]),
        ([2, 6], [2..6]),
        ([6, 8], [6..8]),
        ([4, 5], [4..5]),
        ([7, 9], [7..9]),
        ([3, 7], [3..7]),
        ([4, 6], [4..6]),
        ([4, 8], [4..8])
      ]
    let actual = expand dt
    return $ expected ~=? actual
  ,
  "properly calculate disjoint items" ~: do
    (dt, expected) <-
      [
        (([2..4], [6..8]), [[2..4], [6..8]]),
        (([2..3], [4..5]), [[2..3], [4..5]]),
        (([5..7], [7..9]), [[5..6], [8..9]]),
        (([2..8], [3..7]), [[2, 8], []]),
        (([6..6], [4..6]), [[], [4..5]]),
        (([2..6], [4..8]), [[2..3], [7..8]])
      ]
    let actual = disjoint (fst dt) (snd dt)
    return $ expected ~=? actual
  ,
  "properly calculate joint items" ~: do
    (dt, expected) <-
      [
        (([2..4], [6..8]), []),
        (([2..3], [4..5]), []),
        (([5..7], [7..9]), [7]),
        (([2..8], [3..7]), [3..7]),
        (([6..6], [4..6]), [6]),
        (([2..6], [4..8]), [4..6])
      ]
    let actual = joint (fst dt) (snd dt)
    return $ expected ~=? actual
  ,
  "determine whether a pair overlaps another" ~: do
    (dt, expected) <-
      [
        ([[2..4], [6..8]], False),
        ([[2..3], [4..5]], False),
        ([[5..7], [7..9]], True),
        ([[2..8], [3..7]], True),
        ([[6..6], [4..6]], True),
        ([[2..6], [4..8]], True)
      ]
    let actual = overlaps dt
    return $ expected ~=? actual
  ,
  "determine whether a pair fully contains another" ~: do
    (dt, expected) <-
      [
        ([[2..4], [6..8]], False),
        ([[2..3], [4..5]], False),
        ([[5..7], [7..9]], False),
        ([[2..8], [3..7]], True),
        ([[6..6], [4..6]], True),
        ([[2..6], [4..8]], False)
      ]
    let actual = fullyContains dt
    return $ expected ~=? actual
  ]
