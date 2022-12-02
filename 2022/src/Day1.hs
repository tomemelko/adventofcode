module Day1 where

import Util
import Data.List.Split

parseInput :: String -> [[Integer]]
parseInput s = map (map parseInt . splitOn "\n") $ splitOn "\n\n" s

sumParts :: [[Integer]] -> [Integer]
sumParts = map sum

calcMax :: [[Integer]] -> Integer
calcMax = maximum . sumParts

showDay :: (Integer -> Integer -> IO ()) -> String -> IO ()
showDay printPartResult filename = do
  in_str <- readInput filename
  print $ (calcMax . parseInput) in_str
  -- Part 1
  printPartResult 1 $ (calcMax . parseInput) in_str