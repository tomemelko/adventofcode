module Day8 where

import Util
import Data.Map (Map)
import qualified Data.Map as Map

data SignalPattern = SignalPattern { input :: [String], output :: [String]} deriving Show
type SegmentPossibilities = [Char]
type Digit = [SegmentPossibilities]
type DecidedSegment = Char
type DecidedDigit = [DecidedSegment]

-- Digits are indexed like this:
--  0000 
-- 1    2
-- 1    2
--  3333 
-- 4    5
-- 4    5
--  6666 
-- if this were to be production code I would fix the length of the Digit type to be exactly 7

digitMapping :: Map [Bool] Int
digitMapping = Map.fromList [
    ([True,  True,  True,  False, True,  True,  True ], 0),
    ([False, False, True,  False, False, True,  False], 1),
    ([True,  False, True,  True,  True,  False, True ], 2),
    ([True,  False, True,  True,  False, True,  True ], 3),
    ([False, True,  True,  True,  False, True,  False], 4),
    ([True,  True,  False, True,  False, True,  True ], 5),
    ([True,  True,  False, True,  True,  True,  True ], 6),
    ([True,  False, True,  False, False, True,  False], 7),
    ([True,  True,  True,  True,  True,  True,  True ], 8),
    ([True,  True,  True,  True,  False, True,  True ], 9)
  ]

newDigit :: [SegmentPossibilities]
newDigit = replicate 7 ['a'..'g']

parseInput :: String -> [SignalPattern]
parseInput xs = map ((\t -> SignalPattern {input = fst t, output = (tail . snd) t}) . (break (=="|") . words)) (lines xs)

isValid ::  DecidedDigit -> String -> Bool
isValid dd s = map elem' dd `elem` Map.keys digitMapping where
    elem' :: Char -> Bool
    elem' c = c `elem` s

isValidForAll :: [String] -> DecidedDigit -> Bool
isValidForAll ss dd = foldl (\accum s -> accum && isValid dd s) True ss

-- We know:
-- - len == 2 means it's a 1, which removes those chars from all other segments except 2 and 5, and forces 2 and 5 to be one those two
-- - len == 3 means it's a 7, which removes those chars from all other segments except 0, 2, and 5, and forces 0, 2, and 5 to be one of those three
-- - len == 4 means it's a 4, "" except 1, 2, 3, and 5
-- - len == 7 means it's an 8, which helps not at all
-- - len == 5 means it's one of 2, 3, 5, which allows us to remove invalid configurations
-- - len == 6 means it's one of 0, 6, 9, which allows us to remove invalid configurations

applyFilter :: (Int -> Char -> Bool) -> Digit -> Digit
applyFilter p = zipWith (filter . p) [0..]

buildPred :: [Int] -> String -> Int -> Char -> Bool
buildPred is cs i c = (i `elem` is && c `elem` cs) || (i `notElem` is && c `notElem` cs)

filterPossibilities :: Digit -> String -> Digit
filterPossibilities d s
  | length s == 2 = applyFilter (buildPred [2, 5] s) d
  | length s == 3 = applyFilter (buildPred [0, 2, 5] s) d
  | length s == 4 = applyFilter (buildPred [1, 2, 3, 5] s) d
  | otherwise = d

dropSinglesFromOthers :: Digit -> Digit
dropSinglesFromOthers d
  | any ((==1) . length) d = applyFilter (\i c ->
    -- We want to keep the element if it's one of our one-length possibility strings
    i `elem` (map fst . filter ((==1) . length . snd)) (zip [0..] d) ||
    -- Or if it's not one of the chars in the one-length strings
    c `notElem` (concat . filter ((==1) . length)) d) d
  | otherwise = d

applyOnce :: ([a] -> Bool) -> ([a] -> [a]) -> [[a]] -> [[a]]
applyOnce _ _ [] = []
applyOnce p f (x : xs)
  | p x = f x : xs
  | otherwise = x : applyOnce p f xs

applyOnceToLen2 :: (SegmentPossibilities -> SegmentPossibilities) -> Digit -> Digit
applyOnceToLen2 = applyOnce ((==2) . length)

pickFirst :: Digit -> Digit
pickFirst = applyOnceToLen2 (\x -> [head x])

pickSecond :: Digit -> Digit
pickSecond = applyOnceToLen2 (\x -> [(head . tail) x])

permutate :: Digit -> [DecidedDigit]
permutate d
  | all ((==1) . length) d = [map head d]
  | otherwise = (permutate . dropSinglesFromOthers . pickFirst) d ++ (permutate . dropSinglesFromOthers . pickSecond) d

filterPossibilitiesRound1 :: SignalPattern -> Digit
filterPossibilitiesRound1 = foldl (\d s -> dropSinglesFromOthers (filterPossibilities d s)) newDigit . input

filterPossibilitiesRound2 :: SignalPattern -> Digit -> [DecidedDigit]
filterPossibilitiesRound2 sp d = filter ((isValidForAll . input) sp) (permutate d)

getDecidedDigit :: SignalPattern -> DecidedDigit
getDecidedDigit sp = check (filterPossibilitiesRound2 sp (filterPossibilitiesRound1 sp)) where
  check :: [DecidedDigit] -> DecidedDigit
  check [x] = x
  -- Again if this were production code I know this is _super bad_ practice, but for AoC this is nice way to assert my assumptions
  check xs = error "welp"

decodeDigit :: DecidedDigit -> String -> Int
decodeDigit dd s = unwrap (Map.lookup (map (`elem` s) dd) digitMapping) where
  unwrap :: Maybe Int -> Int
  -- Again if this were production code I know this is _super bad_ practice, but for AoC this is nice way to assert my assumptions
  unwrap Nothing = error "welp2"
  unwrap (Just x) = x

deduceDisplay :: SignalPattern -> Int
deduceDisplay sp = foldl (\l n -> l * 10 + n) 0 (map ((decodeDigit . getDecidedDigit) sp) (output sp))

calcPart1 :: [SignalPattern] -> Int
calcPart1 = count (`elem` [2,4,3,7]) . map length . concatMap output

calcPart2 :: [SignalPattern] -> Int
calcPart2 = sum . map deduceDisplay

showDay :: (Integer -> Int -> IO ()) -> String -> IO ()
showDay printPartResult filename = do
  inStr <- readInput filename
  printPartResult 1 $ (calcPart1 . parseInput) inStr
  printPartResult 2 $ (calcPart2 . parseInput) inStr