module Main( main ) where

import System.Environment

import "adventofcode" Util
-- AoC 2021
import qualified AoC2021Day1
import qualified AoC2021Day2
import qualified AoC2021Day3
import qualified AoC2021Day4
import qualified AoC2021Day5
import qualified AoC2021Day6
import qualified AoC2021Day7
import qualified AoC2021Day8
import qualified AoC2021Day9
import qualified AoC2021Day10
import qualified AoC2021Day11
import qualified AoC2021Day12
import qualified AoC2021Day13
import qualified AoC2021Day14
import qualified AoC2021Day15

-- AoC 2022
import qualified AoC2022Day1
import qualified AoC2022Day2

main :: IO ()
main = do
  args <- getArgs
  case args of 
    [] -> print "Arg required: can be one of '2021', 'all', 'today easy', or 'today hard'"
    ["2021"] -> do
      AoC2021Day1.showDay (printDayResult 1) "AoC2021/inputs/Day1/input.txt"
      AoC2021Day2.showDay (printDayResult 2) "AoC2021/inputs/Day2/input.txt"
      AoC2021Day3.showDay (printDayResult 3) "AoC2021/inputs/Day3/input.txt"
      AoC2021Day4.showDay (printDayResult 4) "AoC2021/inputs/Day4/input.txt"
      AoC2021Day5.showDay (printDayResult 5) "AoC2021/inputs/Day5/input.txt"
      AoC2021Day6.showDay (printDayResult 6) "AoC2021/inputs/Day6/input.txt"
      AoC2021Day7.showDay (printDayResult 7) "AoC2021/inputs/Day7/input.txt"
      AoC2021Day8.showDay (printDayResult 8) "AoC2021/inputs/Day8/input.txt"
      AoC2021Day9.showDay (printDayResult 9) "AoC2021/inputs/Day9/input.txt"
      AoC2021Day10.showDay (printDayResult 10) "AoC2021/inputs/Day10/input.txt"
      AoC2021Day11.showDay (printDayResult 11) "AoC2021/inputs/Day11/input.txt"
      AoC2021Day12.showDay (printDayResult 12) "AoC2021/inputs/Day12/input.txt"
      AoC2021Day13.showDay (printDayResult 13) "AoC2021/inputs/Day13/input.txt"
      AoC2021Day14.showDay (printDayResult 14) "AoC2021/inputs/Day14/input.txt"
      AoC2021Day15.showDay (printDayResult 15) "AoC2021/inputs/Day15/input.txt"
    ["all"] -> do
      -- I really wish there was a way to reference these by string name so I didn't have to copy-paste these lines. Template Haskell looks scary :(
      AoC2022Day1.showDay (printDayResult 1) "AoC2022/inputs/Day1/input.txt"
      AoC2022Day2.showDay (printDayResult 2) "AoC2022/inputs/Day2/input.txt"
    ["today", "easy"] -> AoC2022Day2.showDay (printDayResult 2) "AoC2022/inputs/Day2/input_simple.txt"
    ["today", "hard"] -> AoC2022Day2.showDay (printDayResult 2) "AoC2022/inputs/Day2/input.txt"
    _ -> do print "Invalid command"
    
