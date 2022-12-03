module AoC2022Day1( showDay ) where

import Util
import Data.List
import Data.List.Split

parseInput :: String -> [[Integer]]
parseInput s = map (map parseInt . splitOn "\n") $ splitOn "\n\n" s

sumParts :: [[Integer]] -> [Integer]
sumParts = map sum

calcPart1 :: [[Integer]] -> Integer
calcPart1 = maximum . sumParts

calcPart2 :: [[Integer]] -> Integer
calcPart2 = sum . take 3 . sortBy (flip compare) . sumParts

-- Golfing!
calcPart1Alt :: String -> Integer
calcPart1Alt s = maximum . map (sum . (map parseInt . splitOn "\n")) $ splitOn "\n\n" s

calcPart2Alt :: String -> Integer
calcPart2Alt s = sum . take 3 . sortBy (flip compare) . map (sum . (map parseInt . splitOn "\n")) $ splitOn "\n\n" s

showDay :: (Integer -> Integer -> IO ()) -> String -> IO ()
showDay printPartResult filename = do
  in_str <- readInput filename
  -- Part 1
  printPartResult 1 $ (calcPart1 . parseInput) in_str
  -- Part 2
  printPartResult 2 $ (calcPart2 . parseInput) in_str
  -- Part 1
  printPartResult 1 $ calcPart1Alt in_str
  -- Part 2
  printPartResult 2 $ calcPart2Alt in_str