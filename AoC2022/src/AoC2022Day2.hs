module AoC2022Day2( showDay ) where

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
calcScorePt1 :: String -> Integer
-- <XYZ shape selected> + <win/draw bonus>
calcScorePt1 "A X" = 4
calcScorePt1 "A Y" = 8
calcScorePt1 "A Z" = 3
calcScorePt1 "B X" = 1
calcScorePt1 "B Y" = 5
calcScorePt1 "B Z" = 9
calcScorePt1 "C X" = 7
calcScorePt1 "C Y" = 2
calcScorePt1 "C Z" = 6
calcScorePt1 _ = -1000000000

-- A is Rock
-- B is Paper
-- C is Scissors
-- X is Lose
-- Y is Draw
-- Z is Win
calcScorePt2 :: String -> Integer
-- <XYZ shape selected> + <win/draw bonus>
calcScorePt2 "A X" = 3
calcScorePt2 "A Y" = 4
calcScorePt2 "A Z" = 8
calcScorePt2 "B X" = 1
calcScorePt2 "B Y" = 5
calcScorePt2 "B Z" = 9
calcScorePt2 "C X" = 2
calcScorePt2 "C Y" = 6
calcScorePt2 "C Z" = 7
calcScorePt2 _ = -1000000000

calcPart1 :: [String] -> Integer
calcPart1 = sum . map calcScorePt1

calcPart2 :: [String] -> Integer
calcPart2 = sum . map calcScorePt2

showDay :: (Integer -> Integer -> IO ()) -> String -> IO ()
showDay printPartResult filename = do
  in_str <- readInput filename
  -- Part 1
  printPartResult 1 $ (calcPart1 . parseInput) in_str
  -- Part 2
  printPartResult 2 $ (calcPart2 . parseInput) in_str