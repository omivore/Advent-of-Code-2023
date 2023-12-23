module Main (main) where

import Test.HUnit
import Test.Framework
import Test.Framework.Providers.HUnit
import Solve (addLine, parseLine1, parseLine2)

main :: IO ()
main = defaultMain $ hUnitTestToTests $ TestList [
  "add line works" ~: do
    (dt, expected) <- [ ( [ ('R', 6)
                          , ('D', 5)
                          , ('L', 2)
                          , ('D', 2)
                          , ('R', 2)
                          , ('D', 2)
                          , ('L', 5)
                          , ('U', 2)
                          , ('L', 1)
                          , ('U', 2)
                          , ('R', 2)
                          , ('U', 3)
                          , ('L', 2)
                          , ('U', 2)
                          ]
                        , 62
                        )
                      , ( [ ('R', 461937)
                          , ('D', 56407)
                          , ('R', 356671)
                          , ('D', 863240)
                          , ('R', 367720)
                          , ('D', 266681)
                          , ('L', 577262)
                          , ('U', 829975)
                          , ('L', 112010)
                          , ('D', 829975)
                          , ('L', 491645)
                          , ('U', 686074)
                          , ('L', 5411)
                          , ('U', 500254)
                          ]
                        , 952408144115
                        )
                      ]

    let actual = addLine 1 0 dt
    return $ expected ~=? actual
  ,
  "parse line 1 works" ~: do
    (dt, expected) <- [ ("R 6 (#70c710)", ('R', 6))
                      , ("D 5 (#0dc571)", ('D', 5))
                      , ("L 2 (#5713f0)", ('L', 2))
                      , ("D 2 (#d2c081)", ('D', 2))
                      , ("R 2 (#59c680)", ('R', 2))
                      , ("D 2 (#411b91)", ('D', 2))
                      , ("L 5 (#8ceee2)", ('L', 5))
                      , ("U 2 (#caa173)", ('U', 2))
                      , ("L 1 (#1b58a2)", ('L', 1))
                      , ("U 2 (#caa171)", ('U', 2))
                      , ("R 2 (#7807d2)", ('R', 2))
                      , ("U 3 (#a77fa3)", ('U', 3))
                      , ("L 2 (#015232)", ('L', 2))
                      , ("U 2 (#7a21e3)", ('U', 2))
                      ]
    let actual = parseLine1 dt
    return $ expected ~=? actual
  ,
  "parse line 2 works" ~: do
    (dt, expected) <- [ ("R 6 (#70c710)", ('R', 461937))
                      , ("D 5 (#0dc571)", ('D', 56407))
                      , ("L 2 (#5713f0)", ('R', 356671))
                      , ("D 2 (#d2c081)", ('D', 863240))
                      , ("R 2 (#59c680)", ('R', 367720))
                      , ("D 2 (#411b91)", ('D', 266681))
                      , ("L 5 (#8ceee2)", ('L', 577262))
                      , ("U 2 (#caa173)", ('U', 829975))
                      , ("L 1 (#1b58a2)", ('L', 112010))
                      , ("U 2 (#caa171)", ('D', 829975))
                      , ("R 2 (#7807d2)", ('L', 491645))
                      , ("U 3 (#a77fa3)", ('U', 686074))
                      , ("L 2 (#015232)", ('L', 5411))
                      , ("U 2 (#7a21e3)", ('U', 500254))
                      ]
    let actual = parseLine2 dt
    return $ expected ~=? actual
  ]
