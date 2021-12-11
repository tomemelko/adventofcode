module Main where

import System.Environment

import Util
import qualified Day1
import qualified Day2
import qualified Day3
import qualified Day4
import qualified Day5
import qualified Day6
import qualified Day7
import qualified Day8
import qualified Day9

main :: IO ()
main = do
  args <- getArgs
  case args of 
    [] -> print "Arg required: can be one of 'all', 'today easy', or 'today hard'"
    ["all"] -> do
      -- I really wish there was a way to reference these by string name so I didn't have to copy-paste these lines. Template Haskell looks scary :(
      Day1.showDay (printDayResult 1) "inputs/Day1/input.txt"
      Day2.showDay (printDayResult 2) "inputs/Day2/input.txt"
      Day3.showDay (printDayResult 3) "inputs/Day3/input.txt"
      Day4.showDay (printDayResult 4) "inputs/Day4/input.txt"
      Day5.showDay (printDayResult 5) "inputs/Day5/input.txt"
      Day6.showDay (printDayResult 6) "inputs/Day6/input.txt"
      Day7.showDay (printDayResult 7) "inputs/Day7/input.txt"
      Day8.showDay (printDayResult 8) "inputs/Day8/input.txt"
    ["today", "easy"] -> Day9.showDay (printDayResult 9) "inputs/Day9/input_simple.txt"
    ["today", "hard"] -> Day9.showDay (printDayResult 9) "inputs/Day9/input.txt"
    
