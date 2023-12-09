module Main (main) where

import Test.HUnit
import Test.Framework
import Test.Framework.Providers.HUnit
import Solve (toHistory, differential, deriveHistories, cascadeHistories)

main :: IO ()
main = defaultMain $ hUnitTestToTests $ TestList [
  "input strings split correctly into histories" ~: do
    (dt, expected) <-
      [
        ("0 3 6 9 12 15", [0, 3, 6, 9, 12, 15]),
        ("1 3 6 10 15 21", [1, 3, 6, 10, 15, 21]),
        ("10 13 16 21 30 45", [10, 13, 16, 21, 30, 45])
      ]
    let actual = toHistory dt
    return $ expected ~=? actual
  ,
  "differential of history calculated accurately" ~: do
    (dt, expected) <-
      [
        ([0, 3, 6, 9, 12, 15], [3, 3, 3, 3, 3]),
        ([3, 3, 3, 3, 3], [0, 0, 0, 0]),
        ([0, 0, 0, 0], [0, 0, 0]),
        ([1, 3, 6, 10, 15, 21], [2, 3, 4, 5, 6]),
        ([2, 3, 4, 5, 6], [1, 1, 1, 1]),
        ([1, 1, 1, 1], [0, 0, 0]),
        ([10, 13, 16, 21, 30, 45], [3, 3, 5, 9, 15]),
        ([3, 3, 5, 9, 15], [0, 2, 4, 6]),
        ([0, 2, 4, 6], [2, 2, 2]),
        ([2, 2, 2], [0, 0])
      ]
    let actual = differential dt
    return $ expected ~=? actual
  ,
  "can calculate the full trends of a history" ~: do
    (dt, expected) <-
      [
        (
            [0, 3, 6, 9, 12, 15],
            [
                [0, 3, 6, 9, 12, 15],
                [3, 3, 3, 3, 3],
                [0, 0 ,0, 0]
            ]
        ),
        (
            [1, 3, 6, 10, 15, 21],
            [
                [1, 3, 6, 10, 15, 21],
                [2, 3, 4, 5, 6],
                [1, 1, 1, 1],
                [0, 0, 0]
            ]
        ),
        (
            [10, 13, 16, 21, 30, 45],
            [
                [10, 13, 16, 21, 30, 45],
                [3, 3, 5, 9, 15],
                [0, 2, 4, 6],
                [2, 2, 2],
                [0, 0]
            ]
        )
      ]
    let actual = deriveHistories dt
    return $ expected ~=? actual
  ,
  "can predict next step of history" ~: do
    (dt, expected) <-
      [
        (
            [
                [0, 3, 6, 9, 12, 15],
                [3, 3, 3, 3, 3]
            ],
            18
        ),
        (
            [
                [1, 3, 6, 10, 15, 21],
                [2, 3, 4, 5, 6],
                [1, 1, 1, 1]
            ],
            28
        ),
        (
            [
                [10, 13, 16, 21, 30, 45],
                [3, 3, 5, 9, 15],
                [0, 2, 4, 6],
                [2, 2, 2],
                [0, 0]
            ],
            68
        )
      ]
    let actual = cascadeHistories 0 dt
    return $ expected ~=? actual
  ]
