module Solve where

import Data.Char
import Debug.Trace

data Comparator = Less (Integer -> Integer -> Bool)
                | More (Integer -> Integer -> Bool)
data Result = Next String
            | Judgement Bool
            deriving (Eq, Show)
data Check = Check String Comparator Integer Result deriving (Eq, Show)
data Gate = Gate [Check] Result deriving (Eq, Show)
type Attribute = (String, Integer)
type Item = [Attribute]

instance Show Comparator where
    show (Less _) = "less"
    show (More _) = "more"

instance Eq Comparator where
    Less _ == Less _ = True
    More _ == More _ = True
    Less _ == More _ = False
    More _ == Less _ = False

parseGate :: String -> (String, Gate)
parseGate input =
    let name = takeWhile ('{' /=) input
        inside = wordsWhen (',' ==) $ takeWhile ('}' /=) $ tail (dropWhile ('{' /=) input)
        (testsText, defaultText) = (init inside, last inside)
        tests = map (\x -> parseCheck x) testsText
        defaultResult
            | defaultText == "A" = Judgement True
            | defaultText == "R" = Judgement False
            | otherwise          = Next defaultText
    in (name, Gate tests defaultResult)

wordsWhen :: (Char -> Bool) -> String -> [String]
wordsWhen p s = case dropWhile p s of
                "" -> []
                s' -> w : wordsWhen p s''
                    where (w, s'') = break p s'

parseCheck :: String -> Check
parseCheck input =
    let isComparator s = s == '<' || s == '>'
        (attribute, afterAttr) = break isComparator input
        (comparatorText, afterCompare) = break Data.Char.isDigit afterAttr
        comparator
            | comparatorText == "<" = Less (<)
            | comparatorText == ">" = More (>)
        (valueText, _:resultText) = break (':' ==) afterCompare
        value = read valueText
        result
            | resultText == "A" = Judgement True
            | resultText == "R" = Judgement False
            | otherwise         = Next resultText
    in Check attribute comparator value result

parseItem :: String -> Item
parseItem input =
    let attributeTexts = (wordsWhen (',' ==)) $ tail $ init input
        separate sep s = (takeWhile (sep /=) s, tail $ dropWhile (sep /=) s)
    in map ((\pair -> (fst pair, read $ snd pair)) . (separate '=')) attributeTexts

parseAll :: String -> ([(String, Gate)], [Item])
parseAll input =
    let (gatesText, _:itemsText) = break ("" ==) (lines input)
    in (map parseGate gatesText, map parseItem itemsText)

testItem :: [(String, Gate)] -> Result -> Item -> Bool
testItem _ (Judgement result) _ = result
testItem gates (Next current) item =
    let gate = case lookup current gates of
                 Just search -> search
                 Nothing -> error "gate not found"
    in case evaluateGate gate item of
        Next next -> testItem gates (Next next) item
        Judgement final -> final

evaluateGate :: Gate -> Item -> Result
evaluateGate (Gate [] defaultResult) _ = defaultResult
evaluateGate (Gate (test@(Check _ _ _ res):tests) defaultResult) item
    | passesCheck test item = res
    | otherwise          = evaluateGate (Gate tests defaultResult) item

passesCheck :: Check -> Item -> Bool
passesCheck (Check attr comp val _) item =
    let itemAttr = case lookup attr item of
                     Just search -> search
                     Nothing -> error "attr not found on item"
    in case comp of
        Less lessThan -> lessThan itemAttr val
        More moreThan -> moreThan itemAttr val

ratings :: Item -> Integer
ratings attrs = foldl (+) 0 (map snd attrs)

-- Part 2 Starts Here
data Range = Range Integer Integer deriving Show
type Items = [(String, Range)]

getAllAccepted :: [(String, Gate)] -> Result -> Items -> [Items]
getAllAccepted _ (Judgement True) items = [items]
getAllAccepted _ (Judgement False) items = []
getAllAccepted gates (Next current) items =
    let gate = case lookup current gates of
                 Just search -> search
                 Nothing -> error "gate not found"
        results = evaluateGateWithRange gate items
    in concatMap (uncurry (getAllAccepted gates)) results

evaluateGateWithRange :: Gate -> Items -> [(Result, Items)]
evaluateGateWithRange (Gate [] defaultResult) items = [(defaultResult, items)]
evaluateGateWithRange (Gate (check@(Check _ _ _ res):checks) defaultResult) items =
    let (passed, failed) = splitOnTest check items
    in (res, passed):(evaluateGateWithRange (Gate checks defaultResult) failed)

splitOnTest :: Check -> Items -> (Items, Items)
splitOnTest (Check attr comp val res) items =
    let (Range bottom top) = case lookup attr items of
                     Just search -> search
                     Nothing -> error "attr not found on item"
        (passRange, failRange) = case comp of
            Less lessThan -> ((Range bottom val), (Range val top))
            More moreThan -> ((Range (val + 1) top), (Range bottom (val + 1)))
        otherAttrs = filter (\range -> (fst range) /= attr) items
    in ((attr, passRange):otherAttrs, (attr, failRange):otherAttrs)

scoreAccepted :: [Items] -> Integer
scoreAccepted [] = 0
scoreAccepted (item:items) =
    let possibilities = map (\(Range bot top) -> top - bot) (snd $ unzip item)
        score = foldl (*) 1 possibilities
    in score + (scoreAccepted items)
