module Main (main) where

import Test.HUnit
import Test.Framework
import Test.Framework.Providers.HUnit
import Solve (parseAll, testItem, Check(..), Gate(..), Result(..), Comparator(..))

main :: IO ()
main = defaultMain $ hUnitTestToTests $ TestList [
  "input strings parsed correctly into gates and items" ~: do
    dt <- [ unlines [ "px{a<2006:qkq,m>2090:A,rfg}"
                    , "pv{a>1716:R,A}"
                    , "lnx{m>1548:A,A}"
                    , "rfg{s<537:gd,x>2440:R,A}"
                    , "qs{s>3448:A,lnx}"
                    , "qkq{x<1416:A,crn}"
                    , "crn{x>2662:A,R}"
                    , "in{s<1351:px,qqz}"
                    , "qqz{s>2770:qs,m<1801:hdj,R}"
                    , "gd{a>3333:R,R}"
                    , "hdj{m>838:A,pv}"
                    , ""
                    , "{x=787,m=2655,a=1222,s=2876}"
                    , "{x=1679,m=44,a=2067,s=496}"
                    , "{x=2036,m=264,a=79,s=2244}"
                    , "{x=2461,m=1339,a=466,s=291}"
                    , "{x=2127,m=1623,a=2188,s=1013}"
                    ]
          ]
    let expectedGates = [ ( "px"
                          , (Gate [ (Check "a" (Less (<)) 2006 (Next "qkq"))
                                  , (Check "m" (More (>)) 2090 (Judgement True))
                                  ] (Next "rfg")
                            )
                          )
                        , ( "pv"
                          , (Gate [ (Check "a" (More (>)) 1716 (Judgement False))
                                  ] (Judgement True)
                            )
                          ),
                          ( "lnx"
                          , (Gate [ (Check "m" (More (>)) 1548 (Judgement True))
                                  ] (Judgement True)
                            )
                          )
                        , ( "rfg"
                          , (Gate [ (Check "s" (Less (<)) 537 (Next "gd"))
                                  , (Check "x" (More (>)) 2440 (Judgement False))
                                  ] (Judgement True)
                            )
                          )
                        , ( "qs"
                          , (Gate [ (Check "s" (More (>)) 3448 (Judgement True))
                                  ] (Next "lnx")
                            )
                          )
                        , ( "qkq"
                          , (Gate [ (Check "x" (Less (<)) 1416 (Judgement True))
                                  ] (Next "crn")
                            )
                          )
                        , ( "crn"
                          , (Gate [ (Check "x" (More (>)) 2662 (Judgement True))
                                  ] (Judgement False)
                            )
                          )
                        , ( "in"
                          , (Gate [ (Check "s" (Less (<)) 1351 (Next "px"))
                                  ] (Next "qqz")
                            )
                          )
                        , ( "qqz"
                          , (Gate [ (Check "s" (More (>)) 2770 (Next "qs"))
                                  , (Check "m" (Less (<)) 1801 (Next "hdj"))
                                  ] (Judgement False)
                            )
                          )
                        , ( "gd"
                          , (Gate [ (Check "a" (More (>)) 3333 (Judgement False))
                                  ] (Judgement False)
                            )
                          )
                        , ( "hdj"
                          , (Gate [ (Check "m" (More (>)) 838 (Judgement True))
                                  ] (Next "pv")
                            )
                          )
                        ]
    let expectedItems = [ [("x", 787), ("m", 2655), ("a", 1222), ("s", 2876)]
                        , [("x", 1679), ("m", 44), ("a", 2067), ("s", 496)]
                        , [("x", 2036), ("m", 264), ("a", 79), ("s", 2244)]
                        , [("x", 2461), ("m", 1339), ("a", 466), ("s", 291)]
                        , [("x", 2127), ("m", 1623), ("a", 2188), ("s", 1013)]
                        ]
    expected <- [(expectedGates, expectedItems)]
    let actual = parseAll dt
    return $ expected ~=? actual
  ,
  "items pass through workflow as expected" ~: do
    let gates = [ ( "px"
                  , (Gate [ (Check "a" (Less (<)) 2006 (Next "qkq"))
                          , (Check "m" (More (>)) 2090 (Judgement True))
                          ] (Next "rfg")
                    )
                  )
                , ( "pv"
                  , (Gate [ (Check "a" (More (>)) 1716 (Judgement False))
                          ] (Judgement True)
                    )
                  ),
                  ( "lnx"
                  , (Gate [ (Check "m" (More (>)) 1548 (Judgement True))
                          ] (Judgement True)
                    )
                  )
                , ( "rfg"
                  , (Gate [ (Check "s" (Less (<)) 537 (Next "gd"))
                          , (Check "x" (More (>)) 2440 (Judgement False))
                          ] (Judgement True)
                    )
                  )
                , ( "qs"
                  , (Gate [ (Check "s" (More (>)) 3448 (Judgement True))
                          ] (Next "lnx")
                    )
                  )
                , ( "qkq"
                  , (Gate [ (Check "x" (Less (<)) 1416 (Judgement True))
                          ] (Next "crn")
                    )
                  )
                , ( "crn"
                  , (Gate [ (Check "x" (More (>)) 2662 (Judgement True))
                          ] (Judgement False)
                    )
                  )
                , ( "in"
                  , (Gate [ (Check "s" (Less (<)) 1351 (Next "px"))
                          ] (Next "qqz")
                    )
                  )
                , ( "qqz"
                  , (Gate [ (Check "s" (More (>)) 2770 (Next "qs"))
                          , (Check "m" (Less (<)) 1801 (Next "hdj"))
                          ] (Judgement False)
                    )
                  )
                , ( "gd"
                  , (Gate [ (Check "a" (More (>)) 3333 (Judgement False))
                          ] (Judgement False)
                    )
                  )
                , ( "hdj"
                  , (Gate [ (Check "m" (More (>)) 838 (Judgement True))
                          ] (Next "pv")
                    )
                  )
                ]
    (dt, expected) <-
      [ ([("x", 787), ("m", 2655), ("a", 1222), ("s", 2876)], True)
      , ([("x", 1679), ("m", 44), ("a", 2067), ("s", 496)], False)
      , ([("x", 2036), ("m", 264), ("a", 79), ("s", 2244)], True)
      , ([("x", 2461), ("m", 1339), ("a", 466), ("s", 291)], False)
      , ([("x", 2127), ("m", 1623), ("a", 2188), ("s", 1013)], True)
      ]
    let actual = testItem gates (Next "in") dt
    return $ expected ~=? actual
  ]
