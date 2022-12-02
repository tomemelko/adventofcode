module Util where

import Data.Maybe
import Text.Read
import GHC.Stack

readInput :: String -> IO String
readInput = readFile

formatResultOutput :: (Show a) => Integer -> Integer -> a -> String
formatResultOutput dayNum partNum resultVal = "Day " ++ show dayNum ++ " Part " ++ show partNum ++ " result: " ++ show resultVal

printDayResult :: (Show a) => Integer -> Integer -> a -> IO ()
printDayResult dayNum partNum resultVal = print $ formatResultOutput dayNum partNum resultVal

parseInt :: HasCallStack => String -> Integer
parseInt = fromJust . readMaybe
