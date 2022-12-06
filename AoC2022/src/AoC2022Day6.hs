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

calcPart1 :: String -> Int
calcPart1 s = findString ((getFirstUniq . slidingWindow 4) s) s + 4

showDay :: (Integer -> Int -> IO ()) -> String -> IO ()
showDay printPartResult filename = do
  in_str <- readInput filename
  -- Part 1
  printPartResult 1 $ calcPart1 in_str
  -- Part 2
--   printPartResult 2 $ (calcPart2 . parseInput) in_str
