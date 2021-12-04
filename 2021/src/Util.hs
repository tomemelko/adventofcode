module Util (
  readInput
, parseInt
, formatResultOutput
, printPartResult
, split
) where

readInput :: String -> IO String
readInput = readFile

parseInt :: Read a => String -> a
parseInt = read

formatResultOutput :: (Show a) => Integer -> a -> String
formatResultOutput partNum resultVal = "Part " ++ show partNum ++ " result: " ++ show resultVal

printPartResult :: (Show a) => Integer -> a -> IO ()
printPartResult partNum resultVal = print $ formatResultOutput partNum resultVal
