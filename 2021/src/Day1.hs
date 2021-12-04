module Day1 where

import Util

parseInput :: String -> [Integer]
parseInput s = map parseInt $ lines s

isGreater :: Integer -> Integer -> Integer
isGreater x y
  | y > x     = 1
  | otherwise = 0

countIncreases :: [Integer] -> Integer
countIncreases x = sum $ zipWith isGreater x $ tail x

slidingWindowSum :: Integer -> [Integer] -> [Integer]
slidingWindowSum windowSize list
  | windowSize == 1 = list
  | otherwise       = zipWith (+) list $ slidingWindowSum (windowSize - 1) $ tail list

main = do
  in_str <- readInput "input.txt"
  -- Part 1
  printPartResult 1 $ countIncreases $ parseInput in_str
  -- Part 2
  printPartResult 2 $ countIncreases $ slidingWindowSum 3 $ parseInput in_str
