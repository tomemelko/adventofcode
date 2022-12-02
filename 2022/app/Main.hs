module Main( main ) where

import System.Environment

import Util
import qualified Day1
import qualified Day2

main :: IO ()
main = do
  args <- getArgs
  case args of 
    [] -> print "Arg required: can be one of 'all', 'today easy', or 'today hard'"
    ["all"] -> do
      -- I really wish there was a way to reference these by string name so I didn't have to copy-paste these lines. Template Haskell looks scary :(
      Day1.showDay (printDayResult 1) "inputs/Day1/input.txt"
      Day2.showDay (printDayResult 2) "inputs/Day2/input.txt"
    ["today", "easy"] -> Day2.showDay (printDayResult 2) "inputs/Day2/input_simple.txt"
    ["today", "hard"] -> Day2.showDay (printDayResult 2) "inputs/Day2/input.txt"
    _ -> do print "Invalid command"
    
