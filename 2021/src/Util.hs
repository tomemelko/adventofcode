module Util where

import Data.Maybe
import Text.Read
import GHC.Stack

readInput :: String -> IO String
readInput = readFile

parseInt :: HasCallStack => Read a => String -> a
parseInt = fromJust . readMaybe

charToInt :: Char -> Int
charToInt c = parseInt [c]

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

splitToTuple :: (Char -> Bool) -> String -> (String, String)
splitToTuple p s = go (split p s) where
  go :: [String] -> (String, String)
  go xs = (head xs, (head . tail) xs)

count :: (a -> Bool) -> [a] -> Int
count f = length . filter f

deMaybe :: Eq a => [Maybe a] -> [a]
deMaybe = map fromJust . filter (/= Nothing)
