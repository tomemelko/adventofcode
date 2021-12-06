module Util (
  readInput
, parseInt
, formatResultOutput
, printDayResult
, split
, count
) where

import Data.Maybe
import Text.Read
import GHC.Stack

readInput :: String -> IO String
readInput = readFile

parseInt :: HasCallStack => Read a => String -> a
parseInt = fromJust . readMaybe

formatResultOutput :: (Show a) => Integer -> Integer -> a -> String
formatResultOutput dayNum partNum resultVal = "Day " ++ show dayNum ++ " Part " ++ show partNum ++ " result: " ++ show resultVal

printDayResult :: (Show a) => Integer -> Integer -> a -> IO ()
printDayResult dayNum partNum resultVal = print $ formatResultOutput dayNum partNum resultVal

-- This is a modified `words` that's more generic, and therefore can be used to split lists of strings on strings in addition to strings on chars
split :: (a -> Bool) -> [a] -> [[a]]
split p s = case dropWhile p s of
  [] -> []
  s' -> w : split p s''
    where (w, s'') = break p s'

count :: (a -> Bool) -> [a] -> Int
count f = length . filter f
