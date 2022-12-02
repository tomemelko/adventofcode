module Day2( showDay ) where

import Util
import Data.List.Split

parseInput :: String -> [String]
parseInput = splitOn "\n"

-- A is Rock
-- B is Paper
-- C is Scissors
-- X is Rock
-- Y is Paper
-- Z is Scissors
calcScore :: String -> Integer
-- <XYZ shape selected> + <win/draw bonus>
calcScore "A X" = 4
calcScore "A Y" = 8
calcScore "A Z" = 3
calcScore "B X" = 1
calcScore "B Y" = 5
calcScore "B Z" = 9
calcScore "C X" = 7
calcScore "C Y" = 2
calcScore "C Z" = 6
calcScore _ = -1000000000

calcPart1 :: [String] -> Integer
calcPart1 = sum . map calcScore

showDay :: (Integer -> Integer -> IO ()) -> String -> IO ()
showDay printPartResult filename = do
  in_str <- readInput filename
  -- Part 1
  printPartResult 1 $ (calcPart1 . parseInput) in_str
  -- Part 2
  -- printPartResult 2 $ (calcPart2 . parseInput) in_str