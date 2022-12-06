module AoC2022Day6( showDay ) where

import Util
import Data.List
import Data.Maybe

slidingWindow :: Int -> String -> [String]
slidingWindow len s
  | length s == len = [s]
  | otherwise = take len s : slidingWindow len (tail s)

getFirstUniq :: [String] -> String
getFirstUniq = head . filter (\s -> length s == (length . nub) s)

findString :: (Eq a) => [a] -> [a] -> Int
findString search str = (fromJust . findIndex (isPrefixOf search)) (tails str)

findStart :: Int -> String -> Int
findStart windowSize s = findString ((getFirstUniq . slidingWindow windowSize) s) s + windowSize

calcPart1 :: String -> Int
calcPart1 = findStart 4

calcPart2 :: String -> Int
calcPart2 = findStart 14

showDay :: (Integer -> Int -> IO ()) -> String -> IO ()
showDay printPartResult filename = do
  in_str <- readInput filename
  -- Part 1
  printPartResult 1 $ calcPart1 in_str
  -- Part 2
  printPartResult 2 $ calcPart2 in_str
