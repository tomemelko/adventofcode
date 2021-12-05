module Main where

import System.Environment

import qualified Day1
import qualified Day2
import qualified Day3
import qualified Day4

main :: IO ()
main = do
  args <- getArgs
  case args of 
    [] -> print "Arg required: can be one of 'all', 'today easy', or 'today hard'"
    ["all"] -> do 
      Day1.showDay "inputs/Day1/input.txt"
      Day2.showDay "inputs/Day2/input.txt"
      Day3.showDay "inputs/Day3/input.txt"
      Day4.showDay "inputs/Day4/input.txt"
    ["today", "easy"] -> Day4.showDay "inputs/Day4/input_simple.txt"
    ["today", "hard"] -> Day4.showDay "inputs/Day4/input.txt"
    
