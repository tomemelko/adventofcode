module Main where

import System.Environment

import Util
import qualified Day1

main :: IO ()
main = do
  args <- getArgs
  case args of 
    [] -> print "Arg required: can be one of 'all', 'today easy', or 'today hard'"
    ["all"] -> do
      -- I really wish there was a way to reference these by string name so I didn't have to copy-paste these lines. Template Haskell looks scary :(
      Day1.showDay (printDayResult 1) "inputs/Day1/input.txt"
    ["today", "easy"] -> Day1.showDay (printDayResult 17) "inputs/Day1/input_simple.txt"
    ["today", "hard"] -> Day1.showDay (printDayResult 17) "inputs/Day1/input.txt"
    
